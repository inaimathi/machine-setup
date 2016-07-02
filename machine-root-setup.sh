echo "   Installing network and window-managment basics..."
apt-get install curl x-window-system xmonad

echo "   Installing basics that don't work out of nix..."
apt-get install wicd-curses pmount python-setuptools python-pip python-lxml pacpl mtp-tools

mkdir /nix && chown inaimathi /nix
loadkeys /home/inaimathi/caps2ctrl.map
