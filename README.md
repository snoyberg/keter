Deployment system for Yesod (and other Haskell) web apps.

## Quick Start

Do get Keter up-and-running quickly on an Ubuntu system, run:

    wget -O - https://raw.github.com/snoyberg/keter/master/setup-keter.sh | bash -ex

(Note: you may need to run the above command twice, if the shell exits after
`apt-get` but before running the rest of its instructions.) This will download
and build Keter from source and get it running with a
default configuration.

## Setup

Instructions are for an Ubuntu system. Eventually, I hope to provide a PPA for
this (please contact me if you would like to assist with this). For now, the
following steps should be sufficient:

First, install PostgreSQL

    sudo apt-get install postgresql

Second, build the `keter` binary and place it at `/opt/keter/bin`. To do so,
you'll need to install the Haskell Platform, and can then build with `cabal`.
This would look something like:

    sudo apt-get install haskell-platform
    cabal update
    cabal install keter
    sudo mkdir -p /opt/keter/bin
    sudo cp ~/.cabal/bin/keter /opt/keter/bin

Third, create a Keter config file:

```yaml
# /opt/keter/etc/keter-config.yaml
root: ..
# host: host to bind to
# port: port to listen on
# setuid: myname
# ssl:
#   host:
#   port:
#   key:
#   certificate:
```

Fourth, set up an Upstart job to start `keter` when your system boots.

```
# /etc/init/keter.conf
start on (net-device-up and local-filesystems and runlevel [2345])
stop on runlevel [016]
respawn

console none

exec /opt/keter/bin/keter /opt/keter/etc/keter-config.yaml
```

Finally, start the job for the first time:

    sudo start keter

Optionally, you may wish to change the owner on the `/opt/keter/incoming`
folder to your user account, so that you can deploy without `sudo`ing.

    sudo mkdir -p /opt/keter/incoming
    sudo chown $USER /opt/keter/incoming

## Bundles

An application needs to be set up as a keter bundle. This is a GZIPed tarball
with a `.keter` filename extension and which has one special file:
`config/keter.yaml`. A sample file is:

```yaml
exec: ../dist/build/yesodweb/yesodweb
args:
    - production
host: www.yesodweb.com
ssl: false # true would use https scheme for approot

# Additional hosts your app will listen on, without affecting approot.
extra-hosts:
    - www1.yesodweb.com

# Static file hosts. Keter handles the serving for you.
static-hosts:
    - host: static.yesodweb.com
      root: ../static # relative to config file, just like the executable

# Host name redirects.
redirects:
    - from: yesodweb.com
      to: www.yesodweb.com
```

A sample Bash script for producing a Keter bundle is:

```bash
#!/bin/bash -ex

cabal build
strip dist/build/yesodweb/yesodweb
rm -rf static/tmp
tar czfv yesodweb.keter dist/build/yesodweb/yesodweb config static
```

For users of Yesod, The `yesod` executable provides a `keter` command for
creating the bundle, and the scaffolded site provides a `keter.yaml` file.

## Deploying

In order to deploy, you simply copy the keter bundle to `/opt/keter/incoming`.
To update an app, copy in the new version. The old process will only be
terminated after the new process has started answering requests. To stop an
application, delete the file from incoming.
