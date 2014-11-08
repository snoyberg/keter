#!/bin/bash
NAME=keter
VER=1.3.5
export PATH=$PATH:/var/lib/gems/1.8/bin
FPM=$(which fpm)

set -e

# ensure we have fpm, or try to install as necessary.
if [[ -z "$FPM" ]]; then
    sudo aptitude install ruby-dev gcc rubygems
    sudo gem install fpm
    FPM=$(which fpm)
fi

# ensure keter is there, otherwise install.
if [[ ! -f .cabal-sandbox/bin/$NAME ]]; then
    cabal sandbox init
    cabal update
    cabal install ..
fi

# make folder structure
mkdir -p $NAME-$VER/{bin,etc,init/sysv,init/upstart,var/run/keter}
mkdir -p $NAME-$VER/var/www/keter/{incoming,log,temp}

# copy the keter bin into /bin
cp .cabal-sandbox/bin/$NAME $NAME-$VER/bin/

cd $NAME-$VER
# copy over scripts if missing
if [[ ! -f etc/keter-config.yaml ]]; then 
    cp ../etc/keter-config.yaml etc/keter-config.yaml
fi
if [[ ! -f init/sysv/keter ]]; then
    cp ../init/sysv/keter init/sysv/keter
fi
if [[ ! -f init/upstart/keter ]]; then
    cp ../init/upstart/keter init/upstart/keter
fi

# use fpm to generate the debian package.

# for sysv (debian 6,7)
$FPM -n $NAME -v $VER -t deb --deb-init=init/sysv/keter \
                             --config-files /etc/keter.conf \
                             --deb-user www-data \
                             --deb-group www-data \
                             -s dir bin/keter=/usr/sbin/keter etc/keter-config.yaml=/etc/keter.conf var/www=var

# for upstart (ubuntu?)
# $FPM -n $NAME -v $VER -t deb --deb-init=init/sysv/keter --config-files /opt/keter/etc/keter-config.yaml -s dir bin/keter=/opt/keter/bin/keter etc/keter-config.yaml=/opt/keter/etc/keter-config.yaml
