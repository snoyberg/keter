#!/bin/bash
export PATH=$PATH:/var/lib/gems/1.8/bin
FPM=$(which fpm)

set -e

# ensure we have fpm, or try to install as necessary.
if [[ -z "$FPM" ]]; then
    sudo aptitude install ruby-dev gcc rubygems
    sudo gem install fpm
    FPM=$(which fpm)
fi

sudo aptitude install tar bzip2 -y

mkdir $HOME/build && cd $HOME/build
if [[ ! -d gmp-5.1.3 ]]; then
    wget https://gmplib.org/download/gmp/gmp-5.1.3.tar.bz2
    tar xvjf gmp-5.1.3.tar.bz2
fi
cd gmp-5.1.3
./configure --prefix=$HOME/build/gmp
make
make check
make install
cd ..
/var/lib/gems/1.8/bin/fpm -n libgmp10 -v 5.1.3-keter -t deb -s dir gmp/lib/=/usr/lib
rm -fR gmp-5.1.3
rm -fR gmp
