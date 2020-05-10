dumpkeys | head -1 > /home/inaimathi/caps2ctrl.map
echo 'keycode 58 = Control' >> /home/inaimathi/caps2ctrl.map

eval `ssh-agent -s`
chmod 400 ~/.ssh/id_rsa
ssh-add
echo "Running root installation steps..."
chmod +x machine-root-setup.sh
su -c ./machine-root-setup.sh

echo "Slaughtering CapsLock..."
setxkbmap -layout us -option ctrl:nocaps

echo "Setting up basics..."
guix install dmenu rsync htop emacs git icecat sbcl-next mplayer feh make screen gimp inkscape youtube-dl

echo "Setting up Lisp..."
guix install sbcl
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --load setup.lisp --eval '(quit)'

echo "Setting up languages..."
guix install ghc ocaml polyml

echo "Setting up Python..."
guix install python-setuptools python-pip python-lxml
pip install --user requests cssselect flake8 pylint pyflakes

echo "Setting up Emacs..."
mkdir -p ~/.emacs.d/mine
cp dot-emacs ~/.emacs
cp convenience.el packs.el ~/.emacs.d/mine/

echo "Setting up stumpwm..."
cp stumpwmrc ~/.stumpwmrc

echo "Setting up basic filesystem structure..."
mkdir -p ~/pictures/backgrounds
mkdir -p ~/pictures/screenshots ~/projects ~/downloads ~/books ~/videos ~/bin
mkdir ~/projects

echo "Setting up shell-ui..."
cd ~/projects
git clone git@github.com:Inaimathi/shell-ui.git

cp ~/projects/shell-ui/python/* ~/bin/
cp ~/projects/shell-ui/sh/* ~/bin/
echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc

git config --global user.email "leo.zovic@gmail.com"
git config --global user.name "inaimathi"
git config --global pull.rebase true
