# `machine-setup`

Installation scripts to make setting up a new linux machine easier for me.

Note the **me** in the above sentence. This is highly unlikely to be useful to **you**, except perhaps as a script collection to pillage chunklets of.

## Usage

If you're future me,

1. Put your `id_rsa` and `id_rsa.pub` into `~/.ssh/`
2. Run `machine-setup.sh` from this directory (It will prompt for a root password, and take a looong time. Go get a sandwich or something, but be ready to do a bit or two of authentication/confirmation input)

You should then be able to do all the cool things you want to do with no hassle.

If you're not me ... probably best not to. This script is optimized to my idiosyncratic machine setup (and actually assumes a particular piece of hardware at the moment too). The result will be a system set up to run under Xmonad with no window manager, minimal external volume automation, and an emacs-like set of keystrokes for piloting. You'll want to tear most of it out and replace it with something similarly useful to you.

## Rationale

- `x-window-system` and `xmonad` are installed from the Debian repos because there doesn't seem to be an equivalent in `nix`
- all language runtimes, editing environment programs are installed via `nix-env`
- language-specific libraries are left up to language-specific installation utilities (`stack` for Haskell, `opam` for OCaml, `quicklisp` for Common Lisp, etc)
