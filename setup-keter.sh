#!/bin/bash
set -o errexit -o nounset -o xtrace

# Quick start:
# wget -O - https://raw.github.com/snoyberg/keter/master/setup-keter.sh | bash -ex

LSB_RELEASE=$(lsb_release -sc)

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
echo "deb http://download.fpcomplete.com/ubuntu \"${LSB_RELEASE}\" main"|sudo tee /etc/apt/sources.list.d/fpco.list

sudo apt-get update
sudo apt-get -y install postgresql stack zlib1g-dev

stack update
stack setup
stack install keter

# Call script to create the files needed by keter and then launch keter
bash setup-server.sh
