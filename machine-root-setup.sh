sed -i -e 's/\(deb .*\)/\1 non-free/g' /etc/apt/sources.list
apt-get update
apt-get install firmware-iwlwifi firmware-ralink firmware-realtek
sed -i -e 's/ non-free//g' /etc/apt-sources.list
apt-get update

apt-get install curl x-window-system xmonad

mkdir /nix && chown inaimathi /nix
