-- rich-text.lua --- pandoc reader & writer for rich text <-> Markdown/Org.
--
--   Paste (Asana -> markup):
--     wl-paste -t text/html | pandoc -f rich-text.lua -t gfm
--   Copy (markup -> Asana):
--     pandoc -f gfm -t rich-text.lua --wrap=none doc.md | wl-copy -t text/html
--
-- As a READER (-f) it takes rich-text HTML, e.g. from the clipboard.  Raw
-- string repair runs first: Asana's flat-list markup can only be fixed
-- before parsing, since pandoc's AST has no attribute slot on list items, so
-- the data-list-indent/data-list-type attributes the rebuild relies on are
-- discarded the moment the HTML is parsed.  Everything else -- attribute
-- stripping, tight lists, table-cell flattening, ASCII typography -- happens
-- after parsing, as filters on the document AST, where it can't mangle text
-- that merely looks like markup.
--
-- As a WRITER (-t) it emits HTML prepared for pasting into Asana; see the
-- writer section at the bottom.
--
-- Requires Pandoc >= 3.0.

PANDOC_VERSION:must_be_at_least '3.0'

------------------------------------------------------------------------
--- Raw HTML repair (before parsing): ProseMirror flat lists.
--
-- Asana (and other ProseMirror editors) put every list item on the clipboard
-- as a sibling <li> whose nesting depth and kind live in data-list-indent
-- (counting from 1) and data-list-type attributes rather than in real
-- <ul>/<ol> nesting.  Rebuild proper nesting from those attributes; lists
-- whose items don't all carry data-list-indent are left untouched.

-- Collect each flat <li> of INNER as {indent, tag, body}, or nil if any item
-- lacks data-list-indent (i.e. this is not one of these flat lists).  These
-- <li> never contain other <li>, so a non-greedy match splits them reliably.
local function parse_flat_items (inner)
  local items = {}
  for attrs, body in inner:gmatch('<li(%f[%A][^>]*)>(.-)</li>') do
    local indent = attrs:match('data%-list%-indent="(%d+)"')
    if not indent then return nil end
    local ltype = attrs:match('data%-list%-type="([^"]*)"') or ''
    items[#items + 1] = { indent = tonumber(indent),
                          tag = (ltype == 'bulleted') and 'ul' or 'ol',
                          body = body }
  end
  if #items == 0 then return nil end
  return items
end

-- Walk ITEMS, opening/closing nested lists as the indent changes.  STACK
-- holds the open list tags, innermost first; its length is the depth.  The
-- rebuilt markup includes its own outermost list tags (opened by the first
-- item's indent >= 1), so it replaces the original <ol>/<ul> element whole.
local function rebuild_list (items)
  local out, stack = {}, {}
  local function emit (s) out[#out + 1] = s end
  for _, it in ipairs(items) do
    if it.indent > #stack then
      -- Descend: open new lists inside the still-open parent <li>.
      while #stack < it.indent do
        emit('<' .. it.tag .. '>')
        stack[#stack + 1] = it.tag
      end
    else
      -- Close the previous sibling, then ascend to this item's depth.
      emit('</li>')
      while #stack > it.indent do
        emit('</' .. table.remove(stack) .. '></li>')
      end
      -- Same depth but a different list kind: switch lists.
      if #stack > 0 and stack[#stack] ~= it.tag then
        emit('</' .. table.remove(stack) .. '><' .. it.tag .. '>')
        stack[#stack + 1] = it.tag
      end
    end
    emit('<li>' .. it.body)
  end
  -- Close the final item and every list still open.
  emit('</li>')
  while #stack > 0 do
    emit('</' .. table.remove(stack) .. '>')
    if #stack > 0 then emit('</li>') end
  end
  return table.concat(out)
end

-- Replace each flat <ol>/<ul> element of HTML with properly nested markup.
-- Returning nil from the gsub function keeps a match unchanged, so ordinary
-- lists pass through untouched.  Asana's flat lists never contain nested
-- <ol>/<ul>, so the non-greedy match to the first matching close tag is safe;
-- a genuinely nested list mis-sliced by it fails parse_flat_items and is kept.
local function rebuild_flat_lists (html)
  return (html:gsub('<([ou]l)%f[%A][^>]*>(.-)</%1>',
                    function (tag, inner)
                      local items = parse_flat_items(inner)
                      return items and rebuild_list(items) or nil
                    end))
end

------------------------------------------------------------------------
--- AST tidying (after parsing).
--
-- These clean-ups help rich text from any source, not just Asana.

-- Drop identifiers, classes, and key-value attributes.  Pandoc preserves an
-- unrepresentable attribute by falling back to raw HTML -- e.g. an
-- <a class="..." href="..."> is emitted as a literal tag instead of a
-- Markdown link -- and auto-generated heading identifiers otherwise appear
-- as :PROPERTIES: drawers in Org output.
local function clear_attr (el)
  el.attr = pandoc.Attr()
  return el
end

-- Editors wrap each list item's text in <p> (<li><p>...</p></li>), which
-- makes pandoc emit a "loose" list -- a blank line between every item.
-- Rewrite an item's single paragraph (if that's all the prose it holds) as
-- plain content so the list stays tight; genuinely multi-paragraph items are
-- left alone.
local function tighten (list)
  local content = list.content
  for i = 1, #content do
    local paras = 0
    for _, b in ipairs(content[i]) do
      if b.t == 'Para' then paras = paras + 1 end
    end
    if paras == 1 then
      local item = pandoc.Blocks{}
      for _, b in ipairs(content[i]) do
        item:insert(b.t == 'Para' and pandoc.Plain(b.content) or b)
      end
      content[i] = item
    end
  end
  list.content = content
  return list
end

-- Gather the inline content of BLOCKS as one run, a space between blocks.
-- Empty blocks are dropped, and lists are walked item by item so their
-- entries don't run together.
local function flatten_to_inlines (blocks)
  local out = pandoc.Inlines{}
  local function append (inlines)
    if #inlines == 0 then return end
    if #out > 0 then out:insert(pandoc.Space()) end
    out:extend(inlines)
  end
  for _, b in ipairs(blocks) do
    if b.t == 'BulletList' or b.t == 'OrderedList' then
      for _, item in ipairs(b.content) do
        append(flatten_to_inlines(item))
      end
    else
      append(pandoc.utils.blocks_to_inlines({b}, pandoc.Inlines{}))
    end
  end
  return out
end

-- Flatten BLOCKS to a single inline line.  Used on table cells: any block
-- content or line break in a cell disqualifies the table from being a
-- pipe/Org table, making pandoc emit it as raw HTML instead.  Empty
-- paragraphs are dropped and paragraph breaks become spaces.
local function flatten_blocks (blocks)
  if #blocks == 0 then return blocks end
  local out = pandoc.Inlines{}
  for _, il in ipairs(flatten_to_inlines(blocks)) do
    out:insert((il.t == 'LineBreak' or il.t == 'SoftBreak')
               and pandoc.Space() or il)
  end
  while #out > 0 and out[1].t == 'Space' do out:remove(1) end
  while #out > 0 and out[#out].t == 'Space' do out:remove(#out) end
  if #out == 0 then return pandoc.Blocks{} end
  return pandoc.Blocks{pandoc.Plain(out)}
end

local function flatten_rows (rows)
  for _, row in ipairs(rows) do
    for _, cell in ipairs(row.cells) do
      cell.contents = flatten_blocks(cell.contents)
    end
  end
end

local function tidy_table (tbl)
  flatten_rows(tbl.head.rows)
  for _, body in ipairs(tbl.bodies) do
    flatten_rows(body.head)
    flatten_rows(body.body)
  end
  flatten_rows(tbl.foot.rows)
  tbl.attr = pandoc.Attr()
  return tbl
end

-- Smart typography flattened to plain ASCII equivalents.
local ASCII = {
  ['\u{201C}'] = '"',   ['\u{201D}'] = '"',   -- curly double quotes
  ['\u{201E}'] = '"',   ['\u{201F}'] = '"',   -- low / reversed double quotes
  ['\u{2018}'] = "'",   ['\u{2019}'] = "'",   -- curly single quotes
  ['\u{201A}'] = "'",   ['\u{201B}'] = "'",   -- low / reversed single quotes
  ['\u{2014}'] = '---',                       -- em dash
  ['\u{2013}'] = '--',                        -- en dash
  ['\u{2026}'] = '...',                       -- ellipsis
  ['\u{00A0}'] = ' ',                         -- non-breaking space
}

local function asciify (str)
  local s = str.text
  -- Every character above starts with byte 0xC2 or 0xE2 in UTF-8, so most
  -- Strs can be skipped without running the replacements.
  if not s:find('[\194\226]') then return nil end
  for from, to in pairs(ASCII) do
    s = s:gsub(from, to)
  end
  return pandoc.Str(s)
end

-- Pandoc normalizes most data-* attributes by removing the prefix, except
-- where the resulting name is already an HTML attribute (e.g. data-height).
local function data_attribute (el, name)
  return el.attributes[name] or el.attributes['data-' .. name]
end

-- Pandoc represents <mark> as a Span with a synthetic "mark" class and
-- would otherwise write it as a generic <span> in GFM and drop it from Org.
-- Keep the source representation semantic and portable: Asana's fixed yellow
-- clipboard attributes are an output concern handled by the writer below.
-- Each writer ignores the raw-inline variant intended for the other format.
local function raw_highlight (span)
  local inlines = {
    pandoc.RawInline('html', '<mark>'),
    pandoc.RawInline('org', '@@html:<mark>@@'),
  }
  for _, inline in ipairs(span.content) do
    inlines[#inlines + 1] = inline
  end
  inlines[#inlines + 1] = pandoc.RawInline('html', '</mark>')
  inlines[#inlines + 1] = pandoc.RawInline('org', '@@html:</mark>@@')
  return inlines
end

-- Asana distinguishes a canonical object link from a custom-text link by
-- omitting data-title from the former's clipboard span.  Org and GFM have no
-- native place to retain that distinction, so carry it in a private query
-- parameter and remove the parameter again in the writer.
local ASANA_DYNAMIC_MARKER = '__rich_text_asana_dynamic=1'

local function mark_dynamic_asana_link (target)
  local url, fragment = target:match('^(.-)(#.*)$')
  if not url then url, fragment = target, '' end
  local separator = url:find('?', 1, true) and '&' or '?'
  if url:match('[?&]$') then separator = '' end
  return url .. separator .. ASANA_DYNAMIC_MARKER .. fragment
end

local function unmark_dynamic_asana_link (target)
  local url, fragment = target:match('^(.-)(#.*)$')
  if not url then url, fragment = target, '' end
  local count
  url, count = url:gsub('([?&])' .. ASANA_DYNAMIC_MARKER .. '$', '')
  return url .. fragment, count > 0
end

local function tidy_span (span)
  for _, class in ipairs(span.classes) do
    if class == 'mark' or class:match('highlight') then
      return raw_highlight(span)
    end
  end
  if data_attribute(span, 'asana-object')
      and not data_attribute(span, 'title')
      and #span.content == 1
      and span.content[1].t == 'Link' then
    span.content[1].target =
      mark_dynamic_asana_link(span.content[1].target)
  end
  return span.content
end

-- Asana puts inline images on the clipboard as empty divs carrying asset
-- metadata rather than as <img> elements with a URL.  Represent one as a
-- normal, deliberately broken image whose synthetic URI retains that
-- metadata.  Ending the URI in image.png is significant: it lets the Org
-- reader recognize the bare [[...]] link emitted by its writer as an image
-- again.  Other divs are ordinary editor scaffolding and can be unwrapped.
local function unwrap_div (div)
  for _, class in ipairs(div.classes) do
    if class == 'ProsemirrorEditor-inlineAsset' then
      local asset = data_attribute(div, 'asana-image-asset-id')
      if asset then
        local domain = data_attribute(div, 'asana-image-domain-id') or '-'
        local width = data_attribute(div, 'width') or '-'
        local height = data_attribute(div, 'height') or '-'
        local ratio = data_attribute(div, 'resize-ratio') or '-'
        local uri = ('asana-asset:%s/%s/%sx%s@%s/image.png')
          :format(domain, asset, width, height, ratio)
        return pandoc.Para{pandoc.Image('Asana inline image', uri)}
      end
    end
  end
  return div.content
end

local tidy = {
  -- Web apps use <br> where markup formats want plain flowing text; a line
  -- break anywhere in a cell also disqualifies its table (see above), so
  -- turn hard breaks into spaces everywhere.  (Restrict this to tables by
  -- moving the rule into flatten_blocks, if hard breaks ever matter.)
  LineBreak   = function () return pandoc.Space() end,
  Str         = asciify,
  Link        = clear_attr,
  Image       = clear_attr,
  Header      = clear_attr,
  Code        = clear_attr,
  CodeBlock   = clear_attr,
  Span        = tidy_span,
  Div         = unwrap_div,
  BulletList  = tighten,
  OrderedList = tighten,
  Table       = tidy_table,
}

------------------------------------------------------------------------
--- The reader.
--
-- Parse native <span>/<div> elements so the tidy filter can discard the
-- wrappers web apps litter their markup with while preserving meaningful
-- spans such as highlights.

function Reader (input, _opts)
  local html = tostring(input)
  if html:find('data-list-indent', 1, true) then
    html = rebuild_flat_lists(html)
  end
  return pandoc.read(html, 'html+native_spans+native_divs'):walk(tidy)
end

------------------------------------------------------------------------
--- The writer: Markdown/Org (anything pandoc reads) -> Asana-ready HTML.
--
-- Asana's editor rebuilds pasted links rather than trusting them: anchors
-- from foreign HTML are dropped, and anchors pointing at Asana objects are
-- unfurled into links whose text is the live object name, discarding the
-- text they arrived with.  Its own clipboard content suffers neither fate,
-- so mimic it (as captured from a real Asana copy): wrap links to Asana
-- objects in the editor's data-asana-object <span> markup, and mark the
-- fragment's first element with data-pm-slice -- ProseMirror's editor-native
-- marker, which routes the paste through the trusting parser instead of the
-- sanitizer.  Other paste targets ignore the unknown attributes.

local ASANA_HIGHLIGHT_OPEN =
  '<mark data-highlight-color="yellow"'
  .. ' class="ProsemirrorEditor-highlight'
  .. ' ProsemirrorEditor-highlight--yellow"'
  .. ' style="background-color:'
  .. ' var(--color-richtext-highlight-background, #feedd9);">'

-- Markdown and Org deliberately store a plain semantic <mark>.  Asana's
-- clipboard parser expects the editor-specific form it originally supplied,
-- so restore that form only at the output boundary.
local function asana_highlight (raw)
  if raw.format == 'html' and raw.text == '<mark>' then
    return pandoc.RawInline('html', ASANA_HIGHLIGHT_OPEN)
  end
end

-- The object GID of an app.asana.com PATH: its last whole segment of two or
-- more digits, ignoring query and fragment.  (The leading /0/ or /1/ is the
-- URL format version, not a GID.)  Returns nil if there is none, i.e. the
-- URL doesn't point at an Asana object.
local function asana_gid (path)
  path = path:match('^[^?#]*')
  local gid, pos = nil, 1
  while true do
    local from, to, digits = path:find('/(%d%d+)', pos)
    if not from then return gid end
    local boundary = path:sub(to + 1, to + 1)
    if boundary == '' or boundary == '/' then gid = digits end
    pos = to + 1
  end
end

-- Wrap LINK in the <span> markup Asana's editor itself puts on the
-- clipboard for links to Asana objects: data-object-id is the object's GID
-- and data-preferred-path is the URL sans origin.  Custom-text links also
-- carry data-title; canonical links arrive here with the private marker added
-- by the reader, so omit data-title for them.  Links to anything else are
-- kept as they are.
local function object_link_span (link)
  local target, dynamic = unmark_dynamic_asana_link(link.target)
  local path = target:match('^https?://app%.asana%.com(/.*)$')
  local gid = path and asana_gid(path)
  if not gid then return nil end
  link.target = target
  local attributes = {
    { 'data-asana-object',   '1' },
    { 'data-object-id',      gid },
    { 'data-preferred-path', path },
  }
  if not dynamic then
    attributes[#attributes + 1] =
      { 'data-title', pandoc.utils.stringify(link.content) }
  end
  return pandoc.Span({link}, pandoc.Attr('', {}, attributes))
end

-- Turn the synthetic image produced by the reader back into the empty div
-- Asana placed on the clipboard.  URI fields are deliberately restricted to
-- characters which are safe in quoted HTML attributes.
local function asana_asset_div (block)
  if #block.content ~= 1 or block.content[1].t ~= 'Image' then return nil end
  local domain, asset, width, height, ratio =
    block.content[1].src:match(
      '^asana%-asset:([%w._~-]+)/([%w._~-]+)/([%w._~-]+)x'
      .. '([%w._~-]+)@([%w._~-]+)/image%.png$')
  if not asset then return nil end

  local html = {
    '<div class="ProsemirrorEditor-inlineAsset"',
    (' data-asana-image-asset-id="%s"'):format(asset),
  }
  local function add_attribute (name, value)
    if value ~= '-' then
      html[#html + 1] = (' %s="%s"'):format(name, value)
    end
  end
  add_attribute('data-height', height)
  add_attribute('data-width', width)
  add_attribute('data-resize-ratio', ratio)
  add_attribute('data-asana-image-domain-id', domain)
  html[#html + 1] = '></div>'
  return pandoc.RawBlock('html', table.concat(html))
end

function Writer (doc, opts)
  local html = pandoc.write(doc:walk{
    RawInline = asana_highlight,
    Link      = object_link_span,
    Para      = asana_asset_div,
    Plain     = asana_asset_div,
  }, 'html', opts)
  -- Mark the fragment as editor-native content.
  return (html:gsub('^(%s*<%a[%w]*)', '%1 data-pm-slice="0 0 []"', 1))
end
