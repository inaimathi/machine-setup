# `machine-setup`

Installation scripts to make setting up a new linux machine easier for me.

Note the **me** in the above sentence. This is highly unlikely to be useful to **you**, except perhaps as a script collection to pillage chunklets of.

## Usage

If you're future me,

1. Put your `id_rsa` and `id_rsa.pub` into `~/.ssh/`
2. Run `machine-setup.sh` from this directory (It will prompt for a root password, and take a looong time. Go get a sandwich or something, but be ready to do a bit or two of authentication/confirmation input)

You should then be able to do all the cool things you want to do with no hassle.

If you're not me ... probably best not to. This script is optimized to my idiosyncratic machine setup (and actually assumes a particular piece of hardware at the moment too). The result will be a system set up to run under StumpWM with no underlying desktop environment, minimal external volume automation, and an emacs-like set of keystrokes for piloting. You'll want to tear most of it out and replace it with something similarly useful to you.

## Rationale

- install everything you can from `guix`
- things that don't work/aren't available from guix, install from the Debian repos
- language-specific libraries install via language-specific build tools (`quicklisp`, `opam`, `stack`, `pip`, etc)
