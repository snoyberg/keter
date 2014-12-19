# Keter Configuration Examples


Build all examples:

    $ make

The resulting build will attempt to use the package-db
in the parent directory,

Build Keter app bundle with V1.0 configuration syntax:

    $ make foo1_0

Build Keter websocket app bundle:

    $ make websockets

Build Keter app bundle with V0.4 configuration syntax:

    $ make foo


The resulting bundles are moved into the ./incoming folder and will be
unpacked and served by Keter. You can use `make clean` to clean the
incoming directory and remove all bundles.

