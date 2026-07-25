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
-- Requires pandoc >= 3.0.

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
  BulletList  = tighten,
  OrderedList = tighten,
  Table       = tidy_table,
}

------------------------------------------------------------------------
--- The reader.
--
-- Disabling native_spans/native_divs drops the styling <span>/<div> wrappers
-- web apps litter their markup with (e.g. Asana's data-* mention spans),
-- keeping only the content.

function Reader (input, _opts)
  local html = tostring(input)
  if html:find('data-list-indent', 1, true) then
    html = rebuild_flat_lists(html)
  end
  return pandoc.read(html, 'html-native_spans-native_divs'):walk(tidy)
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
-- clipboard for links to Asana objects: data-object-id is the object's GID,
-- data-preferred-path the URL sans origin, and data-title the link text as
-- plain text.  Links to anything else are kept as they are.
local function object_link_span (link)
  local path = link.target:match('^https?://app%.asana%.com(/.*)$')
  local gid = path and asana_gid(path)
  if not gid then return nil end
  return pandoc.Span({link}, pandoc.Attr('', {}, {
    { 'data-asana-object',   '1' },
    { 'data-object-id',      gid },
    { 'data-preferred-path', path },
    { 'data-title',          pandoc.utils.stringify(link.content) },
  }))
end

function Writer (doc, opts)
  local html = pandoc.write(doc:walk{ Link = object_link_span }, 'html', opts)
  -- Mark the fragment as editor-native content.
  return (html:gsub('^(%s*<%a[%w]*)', '%1 data-pm-slice="0 0 []"', 1))
end
