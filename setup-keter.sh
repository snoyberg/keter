#!/bin/bash
set -o errexit -o nounset -o xtrace

# Quick start:
# wget -O - https://raw.github.com/snoyberg/keter/master/setup-keter.sh | bash -ex

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
echo "deb http://download.fpcomplete.com/ubuntu `lsb_release -sc` main"|sudo tee /etc/apt/sources.list.d/fpco.list

sudo apt-get update
sudo apt-get -y install postgresql stack zlib1g-dev

stack update
stack setup
stack install keter -j1

sudo mkdir -p /opt/keter/bin
sudo cp ~/.local/bin/keter /opt/keter/bin

sudo mkdir -p /opt/keter/etc
cat > /tmp/keter-config.yaml <<EOF
# Directory containing incoming folder, where to store logs, etc. Relative to
# the config file directory.
root: ..

# Keter can listen on multiple ports for incoming connections. These ports can
# have HTTPS either enabled or disabled.
listeners:
    # HTTP
    - host: "*4" # Listen on all IPv4 hosts
      #port: 80 # Could be used to modify port
    # HTTPS
    - host: "*4"
      #port: 443
      key: key.pem
      certificate: certificate.pem

# User to run applications as

# setuid: ubuntu

# Get the user's IP address from x-forwarded-for. Useful when sitting behind a
# load balancer like Amazon ELB.

# ip-from-header: true
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
sudo chown `whoami` /opt/keter/incoming
