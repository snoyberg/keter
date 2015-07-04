Deployment system for web applications, originally intended for hosting Yesod
applications. Keter does the following actions for your application:

* Binds to the main port (usually port 80) and reverse proxies requests to your application based on virtual hostnames.
* Provides SSL support if requested.
* Automatically launches applications, monitors processes, and relaunches any processes which die.
* Provides graceful redeployment support, by launching a second copy of your application, performing a health check, and then switching reverse proxying to the new process.
* Management of log files.

Keter provides many more advanced features and extension points. It allows
configuration of static hosts, redirect rules, management of PostgreSQL
databases, and more. It supports a simple bundle format for applications which
allows for easy management of your web apps.

## Quick Start

To get Keter up-and-running quickly on an Ubuntu system, run:

    wget -O - https://raw.githubusercontent.com/snoyberg/keter/master/setup-keter.sh | bash

(Note: you may need to run the above command twice, if the shell exits after
`apt-get` but before running the rest of its instructions.) This will download
and build Keter from source and get it running with a
default configuration. By default Keter will be set up to support HTTPS and
will require you to provide a key and certificate in `/opt/keter/etc`. You can
disable HTTPS in `/opt/keter/etc/keter-config.yaml` by commenting the certificate
and key lines.

_This approach is not recommended for a production system_. We do not recommend
installing a full GHC toolchain on a production server, nor running such ad-hoc
scripts. This is intended to provide a quick way to play with Keter, especially
for temporary virtual machines. For a production system, we recommend building
the `keter` binary on a separate system, and tracking it via a package manager
or similar strategy.

## Bundling your app for Keter

1.  Modify your web app to check for the `PORT` environment variable, and have
    it listen for incoming HTTP requests on that port. Keter automatically
    assigns arbitrary ports to each web app it manages. The Yesod scaffold
    site is already equipped to read the `PORT` environment variable when
    it is set.

2.  Create a file `config/keter.yaml`. The minimal file just has two settings:

    ```yaml
    exec: ../path/to/executable
    host: mydomainname.example.com
    ```

    See the bundles section below for more available settings.

3.  Create a gzipped tarball with the `config/keter.yaml` file, your
    executable, and any other static resources you would like available to your
    application. This file should be given a `.keter` file extension, e.g.
    `myapp.keter`.

4.  Copy the `.keter` file to `/opt/keter/incoming`. Keter will monitor this
    directory for file updates, and automatically redeploy new versions of your
    bundle.

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

# NB: keter writes logs to /opt/keter/log, but some exceptions occasionally
# escape to standard error. This ensures they show up in system logs.
console output

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

Keter also supports wildcard subdomains and exceptions, as in this
example configuration:

```yaml
exec: ../com.example.app
args:
    - Hello
    - World
    - 1
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

## PostgreSQL support

Keter ships by default with a PostgreSQL plugin, which will handle
management of PostgreSQL databases for your application. To use this,
make the following changes:

* Add the following lines to your `config/keter.yaml` file:

```yaml
plugins:
  postgres: true
```

* Keter can be configured to connect to a remote postgres server using the following syntax:
```yaml
plugins:
  postgres: 
     - server: remoteServerNameOrIP
       port: 1234
```

Different webapps can be configured to use different servers using the above syntax.
It should be noted that keter will prioritize it's own postgres.yaml record for an app. 
So if moving an existing app from a local postgres server to a remote one (or 
switching remote servers), the postgres.yaml file will need to be updated manually. 

Keter will connect to the remote servers using the `postgres` account. This setup 
assumes the remote server's `pg_hba.conf` file has been configured to allow connections
from the keter-server IP using the `trust` method. 

(Note: The `plugins` configuration option was added in v1.0 of the
keter configuration syntax. If you are using v0.4 then use `postgres: true`.
The remote-postgres server syntax was added in v1.4.2.)

* Modify your application to get its database connection settings from the following environment variables:
    * `PGHOST`
    * `PGPORT`
    * `PGUSER`
    * `PGPASS`
    * `PGDATABASE`

* The Yesod scaffold site is already equipped to read these environment
  variables when they are set.

## Known issues

*   There are reports of Keter not working behind an nginx reverse proxy. From
    the reports, this appears to be a limitation in nginx's implementation, not a
    problem with Keter. Keter works fine behind other reverse proxies, including
    Apache and Amazon ELB.

    One possible workaround is to add the following lines to your nginx configuration:

        proxy_set_header Connection "";
        proxy_http_version 1.1;

    This has not yet been confirmed to work in production. If you use this,
    please report either its success or failure back to me.

*   Keter does not handle password-protected SSL key files well.  When provided
    with such a key file, unlike Apache and Nginx, Keter will not pause to ask
    for the password.  Instead, your https connections will merely stall.

    To get around this, you need to create a copy of the key without password
    and deploy this new key:

        openssl rsa -in original.key -out new.key

    (Back up the original key first, just in case.)

## Stanza-based config files

Starting with Keter 1.0, there is an alternate format for application Keter
config files, which allows much more flexibility in defining multiple
functionality for a single bundle (e.g., more than one web app, multiple
redirects, etc). This README will eventually be updated to reflect all various
options. In the meanwhile, please see the following examples of how to use this
file format:

* https://github.com/yesodweb/yesod-scaffold/blob/postgres/config/keter.yml
* https://github.com/snoyberg/keter/blob/master/incoming/foo1_0/config/keter.yaml

## Multiple SSL Certificates

Keter is able to serve different certificates for different hosts,
allowing for the deployment of distinct domains using the same
server. An example `keter-config.yaml` would look like::

```
root: ..
listeners:
  - host: "*4" # Listen on all IPv4 hosts
    port: 80
  - host: 127.0.0.1
    key: key.pem
    certificate: certificate1.pem
  - host: 127.0.0.2
    key: key.pem
    certificate: certificate2.pem
```

## FAQ

*   Keter spawns multiple failing process when run with `sudo start keter`.
    *   This may be due to Keter being unable to find the SSL certificate and key.
        Try to run `sudo /opt/keter/bin/keter /opt/keter/etc/keter-config.yaml`.
        If it fails with `keter: etc/certificate.pem: openBinaryFile: does not exist`
        or something like it, you may need to provide valid SSL certificates and keys
        or disable HTTPS, by uncommenting the key and certificate lines from
        `/opt/keter/etc/keter-config.yaml`.


## Contributing

If you are interested in contributing, see
https://github.com/snoyberg/keter/blob/master/incoming/README.md for a
complete testing workflow. If you have any questions, you can open an
issue in the issue tracker, ask on the #yesod freenode irc channel, or
send an email to yesodweb@googlegroups.com.
