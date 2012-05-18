Deployment system for Yesod (and other Haskell) web apps.

## Setup

Instructions are for an Ubuntu system. Eventually, I hope to provide a PPA for
this (please contact me if you would like to assist with this). For now, the
following steps should be sufficient:

First, install Nginx and PostgreSQL

    sudo apt-get install nginx postgresql

Second, build the `keter` binary and place it at `/usr/bin`. At the time of
writing, a copy of this executable compiled for Ubuntu 12.04 64-bit is
available at:
[http://www.yesodweb.com/static/keter.bz2](http://www.yesodweb.com/static/keter.bz2).
Note that this file may not be available in the future.

Third, set up an Upstart job to start `keter` when your system boots.

```
# /etc/init/keter.conf
start on (net-device-up and local-filesystems and runlevel [2345])
stop on runlevel [016]
respawn

console none

exec /usr/bin/keter /opt/keter
```

Finally, start the job for the first time:

    sudo start keter

## Bundles

An application needs to be set up as a keter bundle. This is a GZIPed tarball
with a `.keter` filename extension and which has one special file:
`config/keter.yaml`. A sample file is:

```yaml
exec: ../dist/build/yesodweb/yesodweb
args:
    - production
host: www.yesodweb.com
```

yesodweb.com uses the following Bash script to create its keter bundle. Going
forward, this will probably be a command available from the `yesod` executable
and part of all scaffolded sites:

```bash
#!/bin/bash -ex

cabal build
strip dist/build/yesodweb/yesodweb
rm -rf static/tmp
tar czfv yesodweb.keter dist/build/yesodweb/yesodweb config static
```

## Deploying

In order to deploy, you simply copy the keter bundle to `/opt/keter/incoming`.
To update an app, copy in the new version. The old process will only be
terminated after the new process has started answering requests. To stop an
application, delete the file from incoming.

## Technical Details

Components:

* Logger: provides a file descriptor to redirect output to. Takes the name of
  the app.

* Process: Give it all the information (executable, working directory,
  environment, args) for a process, and it will start the process and monitor
  it. If the process dies, it will restart. Binds the output to the logger.
  Allows you to terminate the process.

* Postgres: Ask it for database information for an app. If no information is
  available, it will create a database/user.

* Nginx: Send it commands to add/modify/remove a virtual host. It will write
  the config file and reload nginx. Also handles management of the pool of open
  ports to be assigned.

* TempFolder: Wipes out a folder on startup, then assigns random, unique
  folders inside it on request.

* App: Started with a path to an app bundle. Unpacks into a random folder
  inside the temp folder, gets a random port, starts a Process, updates Nginx,
  and waits for commands. Accepts two commands: reload and terminate.

    * Terminate kills the existing process, removes from Nginx, and deletes the
      folder.

    * Reload gets a new random port, unpacks to a new folder in the temp
      folder, starts a Process, waits till the process is "ready" (checks with
      an HTTP request)- canceling after a certain timeout.

        * If process is not ready within timeout, terminate the new process and
          delete the new folder.

        * If a process *is* ready, tell Nginx to use the new port, wait some
          amount of time (20 seconds?), send a TERM to the old process, wait
          some more (1 minute?) and delete the old folder.

* Keter: Delete temp folder, start the Nginx, logger, and port assigner. Start
  a new App for each app in the incoming folder. Monitor for file changes in
  the incoming folder, and appropriately start a new app, reload an existing app,
  or delete an existing app. Also provide a web interface based on logger.
