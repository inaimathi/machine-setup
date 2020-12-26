set -e

if [ ! -f checkpoints/001-root.check ]
then
   echo "   Installing window-managment basics..."
   apt-get install stumpwm
   touch checkpoints/001-root.check
fi

if [ ! -f checkpoints/002-root.check ]
then
   echo "   Installing guix..."
   wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
   sh guix-install.sh
   touch checkpoints/002-root.check
fi

if [ ! -f checkpoints/003-root.check ]
then
   echo "   Installing basics that don't work out of guix..."
   apt-get install wicd-curses pmount pacpl mtp-tools libblas-dev liblapack-dev gnupg gnupg-agent
   touch checkpoints/003-root.check
fi

loadkeys /home/inaimathi/caps2ctrl.map
