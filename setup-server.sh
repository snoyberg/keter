#!/bin/bash
set -o errexit -o nounset -o xtrace

# Quick start:
# wget -O - https://raw.github.com/snoyberg/keter/master/setup-keter-server.sh | bash -ex

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
      port: 80 # Could be used to modify port
    # HTTPS
    - host: "*4"
      port: 443
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

cat > /tmp/keter.service <<EOF
[Unit]
Description=Keter Web Server
Documentation=man:keter(1p) http://github.com/snoyberg/keter
After=network.service

[Service]
ExecStart=/opt/keter/bin/keter /opt/keter/etc/keter-config.yaml

[Install]
WantedBy=multi-user.target
EOF
sudo chown root:root /tmp/keter.service
sudo mv /tmp/keter.service /etc/systemd/system/

systemctl enable keter
systemctl start keter

sudo mkdir -p /opt/keter/incoming
sudo chown "$(whoami)" /opt/keter/incoming
