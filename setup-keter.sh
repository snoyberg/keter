#!/bin/bash
set -o errexit -o nounset -o xtrace

# Quick start:
# wget -O - https://raw.github.com/snoyberg/keter/master/setup-keter.sh | bash -ex

REQMEM=1024
MEM=`free -m | grep '^Mem' | awk '{print $2}'`
MAYBESWAP=`free -m | grep '^Swap' | awk '{print $2}'`

if [ -z $MAYBESWAP ]
then
  SWAP=0
else
  SWAP=$MAYBESWAP
fi

MEMSWAP=$(($MEM+$SWAP))

if [ $REQMEM -gt $MEMSWAP ]
then
  echo "Making swap (THIS WILL MAKE INSTALL SLOWER, BUT PREVENT CRASH"
  dd if=/dev/zero of=/keterswap bs=1M count=1024
  mkswap /keterswap
  swapon /keterswap
fi

sudo apt-get update
sudo apt-get install postgresql haskell-platform -y

cabal update
cabal install keter
sudo mkdir -p /opt/keter/bin
sudo cp ~/.cabal/bin/keter /opt/keter/bin

sudo mkdir -p /opt/keter/etc
cat > /tmp/keter-config.yaml <<EOF
# /opt/keter/etc/keter-config.yaml
root: ..
setuid: $USER
# host: host to bind to
# port: port to listen on
# ssl:
#   host:
#   port:
#   key:
#   certificate:
EOF
sudo chown root:root /tmp/keter-config.yaml
sudo mv /tmp/keter-config.yaml /opt/keter/etc

cat > /tmp/keter.conf <<EOF
# /etc/init/keter.conf
start on (net-device-up and local-filesystems and runlevel [2345])
stop on runlevel [016]
respawn

console none

exec /opt/keter/bin/keter /opt/keter/etc/keter-config.yaml
EOF
sudo chown root:root /tmp/keter.conf
sudo mv /tmp/keter.conf /etc/init

sudo start keter

sudo mkdir -p /opt/keter/incoming
sudo chown "$USER" /opt/keter/incoming
