echo "   Adding non-free repos :(..."
sed -i -e 's/\(deb .*\)/\1 non-free/g' /etc/apt/sources.list
apt-get update
echo "   Installing wifi firmware..."
apt-get install firmware-iwlwifi firmware-ralink firmware-realtek
echo "   Removing non-free repos :)..."
sed -i -e 's/ non-free//g' /etc/apt/sources.list
apt-get update

echo "   Installing network and window-managment basics..."
apt-get install curl x-window-system xmonad

echo "   Installing basics that don't work out of nix..."
apt-get install wicd-curses pmount python-setuptools python-pip python-lxml pacpl

mkdir /nix && chown inaimathi /nix
loadkeys /home/inaimathi/caps2ctrl.map
