# Building debian packages

We provide two scripts to assist in building debian packages.

## Requirements

The `mkketerdeb.sh` script requires `cabal` with sandbox support.
But will try to install the other dependencies on it's own.

## Building keter-X.Y.Z.deb

1. Download a recent keter release from <https://github.com/snoyberg/keter/releases>

2. Extract

3. Run the packaging script.
   ```{sh}
   keter $ cd packaging
   keter/packaging $ ./mkketerdeb.sh
   ```
   This should produce a `.deb` file in the `keter-X.Y.Z` folder.  
   *Note*: if the above command fails, try running it again.

## Building libgmp if not avaiable.

Similarly to building keter, `mklibgmp10deb.sh` can be used to build a `.deb` package of `libgmp10` on which keter depends.
This should only be necessary for *Debian __6__*.
