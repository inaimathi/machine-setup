set -e

checkpoint()
{
    local checkpoint_name="$1"
    shift
    if [ ! -f "checkpoints/$checkpoint_name.check" ]
    then
	echo "$@"
	touch "checkpoints/$checkpoint_name.check"
    else
	echo "Skipping checkpoint $checkpoint_name ..."
    fi
}

if [ ! -d checkpoints ]
then
    mkdir checkpoints
fi

if [ ! -f ~/caps2ctrl.map ]
then
   dumpkeys | head -1 > ~/caps2ctrl.map
   echo 'keycode 58 = Control' >> ~/caps2ctrl.map
fi

if [ ! -f checkpoints/001.check ]
then
    eval `ssh-agent -s`
    chmod 400 ~/.ssh/id_rsa
    ssh-add
    echo "Running root installation steps..."
    chmod +x machine-root-setup.sh
    su -c ./machine-root-setup.sh || sudo ./machine-root-setup.sh
    touch checkpoints/001.check
fi

if [ ! -f checkpoints/002.check ]:
then
    echo ".bashrc changes..."
    setxkbmap -layout us -option ctrl:nocaps
    echo "setxkbmap -layout us -option ctrl:nocaps" >> ~/.bashrc
    touch checkpoints/002.check
fi

if [ ! -f checkpoints/003.check ]:
then
    echo "Setting up basics..."
    mkdir -p ~/.emacs.d/elpa/gnupg
    gpg --homedir ~/.emacs.d/elpa/gnupg --keyserver hkp://keyserver.ubuntu.com  --recv-keys 645357D2883A0966
    guix install dmenu dunst rsync btop kitty emacs git mplayer feh make screen acpi obs
    guix install gimp inkscape icecat youtube-dl
    echo "Keeping underlying LISP version of sbcl (installed with stumpwm earlier)..."
    # echo ")Setting up Lisp..."
    # guix install sbcl
    touch checkpoints/003.check
fi

if [ ! -d ~/quicklisp ]
then
    curl -O https://beta.quicklisp.org/quicklisp.lisp || wget https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --load setup.lisp --eval '(quit)'
fi

if [ ! -f checkpoints/004.check ]
then
    echo "Setting up languages..."
    guix install ghc ocaml polyml
    touch checkpoints/004.check
fi

if [ ! -f checkpoints/005.check ]
then
    echo "Setting up Python..."
    guix install python-setuptools python-pip python-lxml
    guix install python-requests python-cssselect python-flake8 python-pylint python-pyflakes python-black python-lxml python-selenium
    touch checkpoints/005.check
fi

if [ ! -d ~/.emacs.d/mine ]
then
    echo "Setting up Emacs..."
    mkdir -p ~/.emacs.d/mine
    cp emacs/* ~/.emacs.d/mine/
    wget https://raw.githubusercontent.com/jorgenschaefer/pyvenv/master/pyvenv.el
    mv pyvenv.el ~/.emacs.d/mine/
    mkdir -p ~/.emacs.d/elpa
fi

if [ ! -f ~/.emacs ]
then
    cp dot-emacs ~/.emacs
fi

if [ ! -f ~/.stumpwmrc ]
then
    echo "Setting up stumpwm..."
    cp stumpwmrc ~/.stumpwmrc
fi

if [ ! -f checkpoints/006.check ]
then
    echo "Setting up basic filesystem structure..."
    mkdir -p ~/pictures/backgrounds
    mkdir -p ~/pictures/screenshots ~/projects ~/downloads ~/books ~/videos ~/bin
    touch checkpoints/006.check
fi

if [ ! -f checkpoints/007.check ]
then
    echo "Double-check non-root docker stuff..."
    docker run hello-world
    touch checkpoints/007.check
fi

if [ ! -d ~/projects ]
then
    mkdir ~/projects
fi

if [ ! -d ~/projects/shell-ui ]
then
    echo "Setting up shell-ui..."
    cd ~/projects
    git clone git@github.com:Inaimathi/shell-ui.git

    cp ~/projects/shell-ui/python/* ~/bin/
    cp ~/projects/shell-ui/sh/* ~/bin/
    echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc
    echo 'export PATH="~/.local/bin:$PATH"' >> ~/.bashrc
    echo 'GUIX_PROFILE="/home/inaimathi/.guix-profile"' >> ~/.bashrc
    echo '. "$GUIX_PROFILE/etc/profile"'>> ~/.bashrc
fi

git config --global user.email "leo.zovic@gmail.com"
git config --global user.name "inaimathi"
git config --global pull.rebase true
