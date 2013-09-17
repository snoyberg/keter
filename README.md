Deployment system for Yesod (and other Haskell) web apps.

## Quick Start

Do get Keter up-and-running quickly on an Ubuntu system, run:

    wget -O - https://raw.github.com/snoyberg/keter/master/setup-keter.sh | bash

(Note: you may need to run the above command twice, if the shell exits after
`apt-get` but before running the rest of its instructions.) This will download
and build Keter from source and get it running with a
default configuration.

_This approach is not recommended for a production system_. We do not recommend
installing a full GHC toolchain on a production server, nor running such ad-hoc
scripts. This is intended to provide a quick way to play with Keter, especially
for temporary virtual machines. For a production system, we recommend building
the `keter` binary on a separate system, and tracking it via a package manager
or similar strategy.

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

Third, create a Keter config file. You can view a sample at
https://github.com/snoyberg/keter/blob/master/etc/keter-config.yaml.

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
`config/keter.yaml`. A sample file is available at
https://github.com/snoyberg/keter/blob/master/incoming/foo1_0/config/keter.yaml.

Keter as well supports wildcard subdomains and exceptions, as in this example
configuration:

```yaml
exec: ../com.example.app
args:
    - Hello World 1
host: www.example.com
extra-hosts:
    - "*.example.com"
    - foo.bar.example.com
static-hosts:
    - host: static.example.com
      root: ../static
redirects:
    - from: example.com
      to: www.example.com
```

Due to YAML parsing, wildcard hostnames will need to be quoted as above.
Wildcard hostnames are not recursive, so `foo.bar.example.com` must be
explicitly added as an extra hostname in the above example, or
alternatively, `*.*.example.com` would cover all host names two levels
deep. It would not cover host names only one level deep, such as
`qux.example.com`. In this manner, wildcard hostnames correspond to the
manner in which SSL certificates are handled per RFC2818. Wildcards may
be used in only one level of a hostname, as in `foo.*.example.com`.

Full RFC2818 compliance is not present - `f*.example.com` will not be
handled as a wildcard with a prefix.

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

## Known issues

* There are reports of Keter not working behind an nginx reverse proxy. From
  the reports, this appears to be a limitation in nginx's implementation, not a
  problem with Keter. Keter works fine behind other reverse proxies, including
  Apache and Amazon ELB.
