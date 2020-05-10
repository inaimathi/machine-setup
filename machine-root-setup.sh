echo "   Installing window-managment basics..."
apt-get install stumpwm

echo "   Installing guix..."
wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
sh guix-install.sh

echo "   Installing basics that don't work out of guix..."
apt-get install wicd-curses pmount pacpl mtp-tools libblas-dev liblapack-dev gnupg gnupg-agent

loadkeys /home/inaimathi/caps2ctrl.map
