#!/bin/bash
mkdir -p /opt/keter/bin
cp ~/.local/bin/keter /opt/keter/bin

mkdir -p /opt/keter/etc
cat > /tmp/keter-config.yaml <<EOF
# Directory containing incoming folder, where to store logs, etc. Relative to
# the config file directory.
root: ..

# Keter can listen on multiple ports for incoming connections. These ports can
# have HTTPS either enabled or disabled.
listeners:
    # HTTP
    - host: "!6" # Listen on all IPv4 hosts
      port: 80 # Could be used to modify port
    # HTTPS
    - host: "!6"
      port: 443
      key: key.pem
      certificate: certificate.pem

# User to run applications as

# setuid: ubuntu

# Get the user's IP address from x-forwarded-for. Useful when sitting behind a
# load balancer like Amazon ELB.

# ip-from-header: true
EOF
chown root:root /tmp/keter-config.yaml
mv /tmp/keter-config.yaml /opt/keter/etc

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
chown root:root /tmp/keter.service
mv /tmp/keter.service /etc/systemd/system/

systemctl enable keter
systemctl start keter

mkdir -p /opt/keter/incoming
chown "$(whoami)" /opt/keter/incoming
