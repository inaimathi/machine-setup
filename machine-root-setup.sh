echo "   Installing network and window-managment basics..."
apt-get install curl
apt-get install stumpwm

echo "   Installing basics that don't work out of nix..."
apt-get install wicd-curses pmount python-setuptools python-pip python-lxml pacpl mtp-tools libblas-dev liblapack-dev gnupg gnupg-agent

mkdir /nix && chown inaimathi /nix
loadkeys /home/inaimathi/caps2ctrl.map
