## Basics
ssh-add
su -c sh machine-root-setup.sh

setxkbmap -layout us -option ctrl:nocaps

curl https://nixos.org/nix/install | sh

nix-env -i dmenu alsa-utils pumount
nix-env -i emacs-24.5 git firefox

## Lisp stuff
nix-env -i sbcl
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --load setup.lisp

## Haskell/ML stuff
nix-env -f "<nixpkgs>" -iA haskellPackages.stack
stack build

nix-env -i ocaml opam

## Emacs stuff
mkdir ~/.emacs.d
cp dot-emacs ~/.emacs
cp convenience.el ~/.emacs.d/
emacs --batch setup.el

## Basic filesystem/convenience stuff
mkdir -p ~/pictures/backgrounds
mkdir ~/pictures/screenshots ~/projects ~/downloads ~/books ~/videos ~/bin

cd ~/projects
git clone git@github.com:Inaimathi/shell-ui.git
cd ~/

cp ~/projects/shell-ui/python/* ~/bin/
