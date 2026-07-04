# kzar / emacs.d

## Intro

My personal Emacs configuration. Mostly here for my own convenience although feel free to have a poke around.

## Requirements

- Emacs >=30
- [`ripgrep`](https://github.com/BurntSushi/ripgrep)
- [`fd`](https://github.com/sharkdp/fd)
- [`hunspell`](https://github.com/hunspell/hunspell) (and an `en_GB` dictionary)

### Language servers

- [`clangd`](https://clangd.llvm.org/)
- [`typescript-language-server`](https://www.npmjs.com/package/typescript-language-server) (and `typescript`)
- [`rust-analyzer`](https://rust-analyzer.github.io/)

### Rich text <-> Markdown/Org conversion

- [`pandoc`](https://pandoc.org/)
- [`wl-clipboard`](https://github.com/bugaevc/wl-clipboard) or [`xclip`](https://github.com/astrand/xclip) on Linux

## Usage

Again more for me but here's how I use this repo:

      git clone git@github.com:kzar/emacs.d.git ~/.emacs.d
      cp ~/.emacs.d/my-secrets.el.sample ~/.emacs.d/my-secrets.el

## Resources

 - [How I install Emacs on Linux](https://kzar.co.uk/blog/2020/04/14/how-i-install-emacs-on-linux) - A blog post I wrote about setting up Emacs, a little out of date now.
 - [Mastering Emacs articles](https://www.masteringemacs.org/all-articles) - Some great articles about using Emacs, in particular the "What's New in Emacs X" series.
 - [davemail](https://github.com/kzar/davemail) - My Notmuch email setup that integrates with the Emacs config.
