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
   apt-get install pmount pacpl mtp-tools libblas-dev liblapack-dev gnupg gnupg-agent python3-pip slock pavucontrol curl
   touch checkpoints/003-root.check
fi

# if [ ! -f checkpoints/004-root.check ]
# then
#     echo "   Adding known-good DNS servers..."
#     for i in `ip a | grep -oP "^[0-9]: \K(\w+)(?:)"`;
#     do if [ $i != "lo" ]
#        then sudo systemd-resolve --interface=$i --set-dns=8.8.8.8
#        fi;
#     done
#     touch checkpoints/004-root.check
# fi

if [ ! -f checkpoints/005-root.check ]
then
    echo "   Setting up docker related root stuff..."
    curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey | gpg --dearmor -o /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg
    curl -s -L https://nvidia.github.io/libnvidia-container/stable/deb/nvidia-container-toolkit.list | sed 's#deb https://#deb [signed-by=/usr/share/keyrings/nvidia-container-toolkit-keyring.gpg] https://#g' | tee /etc/apt/sources.list.d/nvidia-container-toolkit.list
    apt update
    apt install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
    apt install nvidia-container-toolkit
    usermod -aG docker inaimathi
    newgrp docker
    nvidia-ctk runtime configure
    service docker restart
fi

loadkeys /home/inaimathi/caps2ctrl.map
