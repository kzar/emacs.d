# kzar / emacs.d

## Intro

My personal Emacs configuration. Mostly here for my own convenience although feel free to have a poke around.

## Requirements

- Emacs >=29
- [`ripgrep`](https://github.com/BurntSushi/ripgrep)
- [`fd`](https://github.com/sharkdp/fd)
- [`hunspell`](https://github.com/hunspell/hunspell) (and an `en_GB` dictionary)

## Usage

Again more for me but here's how I use this repo:

      git clone git@github.com:kzar/emacs.d.git ~/.emacs.d
      cp ~/.emacs.d/my-secrets.el.sample ~/.emacs.d/my-secrets.el

## Resources

 - [railwaycat/homebrew-emacsport](https://github.com/railwaycat/homebrew-emacsmacport) - The best Emacs port for Mac that I've found.
 - [Aaron Bedra's Emacs configuration](http://www.aaronbedra.com/emacs.d/) - A much more comprehensive, better written and documented example of an Emacs configuration. (Some bits in my configuration are from this.)
 - [Declaring .emacs bankruptcy](http://emacsblog.org/2007/10/07/declaring-emacs-bankruptcy/) - Good post that inspired me to sort out and share my Emacs configuration. Previously I had one large .emacs file and a bunch of random .el files scattered around my system!
