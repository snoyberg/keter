#!/bin/bash
NAME=keter
VER=1.3.6
export PATH=$PATH:/var/lib/gems/1.8/bin
FPM=$(which fpm)
STACK=$(which stack)
CABAL=$(which cabal)

set -e

# ensure we have fpm, or try to install as necessary.
if [[ -z "$FPM" ]]; then
    sudo aptitude install ruby-dev gcc rubygems
    sudo gem install fpm
    FPM=$(which fpm)
fi

# Build project
# Make sure either stack or cabal is present.
if [[ ! -x "$STACK" ]]; then
    if [[ ! -x "$CABAL" ]]; then
        echo "Error: You need either cabal or stack"
        exit
    else
        if [[ ! -f $($CABAL exec which $NAME) ]]; then
            echo Building using cabal sandboxes...
            $CABAL sandbox init
            $CABAL update
            $CABAL install ..
            echo done
        fi
    fi
else
    if [[ ! -f $($STACK exec which $NAME) ]]; then
        echo Building using stack...
        $STACK build
        echo done
    fi
fi

# make folder structure
echo copying files
mkdir -p $NAME-$VER/{bin,etc,init/sysv,init/upstart,var/run/keter}
mkdir -p $NAME-$VER/var/www/keter/{incoming,log,temp}

# copy the keter binary into /bin
if [[ -f $($STACK exec which $NAME) ]]; then
    cp $($STACK exec which $NAME) $NAME-$VER/bin/
else
    if [[ -f $($CABAL exec which $NAME) ]]; then
        cp $($CABAL exec which $NAME) $NAME-$VER/bin/
    else
        echo Error: Something went wrong. Could not find the built executable.
        exit
    fi
fi

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
echo building deb file
# for sysv (debian 6,7)
$FPM -n $NAME -v $VER -t deb --deb-init=init/sysv/keter \
                             --config-files /etc/keter.conf \
                             --deb-user www-data \
                             --deb-group www-data \
                             -s dir bin/keter=/usr/sbin/keter etc/keter-config.yaml=/etc/keter.conf var/www=var

# for upstart (ubuntu?)
# $FPM -n $NAME -v $VER -t deb --deb-init=init/sysv/keter --config-files /opt/keter/etc/keter-config.yaml -s dir bin/keter=/opt/keter/bin/keter etc/keter-config.yaml=/opt/keter/etc/keter-config.yaml
echo DONE!
