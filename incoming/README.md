# Keter Configuration Examples

Build all examples:

```sh
make
```

The resulting build will attempt to use the `cabal.sandbox.config` in
the parent directory to locate packages for the examples.

The resulting bundles are moved into the ./incoming folder and will be
unpacked and served by Keter. You can use `make clean` to clean the
incoming directory and remove all bundles.

Build Keter app bundle with V1.0 configuration syntax:

```sh
make foo1_0
```

Build Keter websocket app bundle:

```sh
make websockets
```

Build Keter app bundle with V0.4 configuration syntax:

```sh
make foo
```

## Example Testing Workflow

### 1) Build and run keter

```sh
cd keter/
cabal build
./dist/build/keter/keter etc/keter-config.yaml
```

*Using postgresql features requires sudo access*.

### 2) Modify and build bundles

You can modify test bundles in the `incoming/` directory:

```sh
cd keter/incoming
# edit foo1_0/etc/keter.yaml
```

Next, rebuild your changes:

```sh
make   # or `make foo1_0`
```

### 3) Monitor the keter logs

```sh
tail -f log/keter/current.log
```

### 4) Test requests to the new bundle

Use `curl` to test requests to an app:

```sh
http://keter1_0
```

*n.b.* Make sure add keter1_0 to your /etc/hosts file

`incoming/foo1_0` contains a complete example of the v1.0
configuration. `incoming/foo` is the v0.4 configuration and is used to
test compatibility with older versions of the keter app bundle
configuration.
