## TODO
#  - langnostic
#  - static
#  - asdf-archives
#  - certbot
#  - pronounce
#  - kicktracker
#  - lispwiki

## Set all the above up, but realistically, clean out static and decomission everything other than langnostic, certbot, pronounce and kicktracker

apt install openjdk-17-jdk screen git nginx certbot snapd rsync
useradd -m -d /home/inaimathi -s /bin/bash inaimathi
passwd inaimathi
usermod -aG sudo inaimathi
mkdir /home/inaimathi/.ssh
echo "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCwgL2DV6v2mqL3AFBeYPTIIB2A/Bc6yjD2B2GmeNyzCyLES6bLTLml0mjO09rbnoyxFx8u5H7dzHxB7oKL4l6w0XASDOSJkhAInq+lhW/6g3xeDnMnyfeeP5PnhwBaFE5ftBovCsR1MnjSQI5QsnDZWsE7mab/Q544HG4hSPZxLLxMZ2fJK12ESnJkD6oG+XWi15wNK5acAiM84kMbNCnHHv3+IDEiaMKHZ9xVoFRT9wSbVRrruoG1BW1uxTShMR9LRQAvfUM54FDP5RWkK/zmksevVEJ6I2WgWWULBK9Jb4Mx9NmpOk2GKbA3Hx1mXV2sb4/q9LFPwe7NpDmOxay1" > /home/inaimathi/.ssh/authorized_keys
chown -R inaimathi:inaimathi /home/inaimathi/.ssh
chmod 700 /home/inaimathi/.ssh
chmod 600 /home/inaimathi/.ssh/authorized_keys
