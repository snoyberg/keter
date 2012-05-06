Deployment system for Yesod (and other Haskell) web apps.

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
