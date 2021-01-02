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
   gpg --keyserver pool.sks-keyservers.net --recv-keys 3CE464558A84FDC69DB40CFB090B11993D9AEBB5
   wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
   sh guix-install.sh
   touch checkpoints/002-root.check
fi

if [ ! -f checkpoints/003-root.check ]
then
   echo "   Installing basics that don't work out of guix..."
   apt-get install pmount pacpl mtp-tools libblas-dev liblapack-dev gnupg gnupg-agent python3-pip
   touch checkpoints/003-root.check
fi

loadkeys /home/inaimathi/caps2ctrl.map
