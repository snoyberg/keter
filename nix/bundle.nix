/* This makes a keter bundle as described on the github page:
   https://github.com/snoyberg/keter#bundling-your-app-for-keter

   Note that we're not actually putting in the app into the bundle,
   we already can use the nix store for copying, so we just
   symlink to the app.
*/
{ domain,
  exePkg,
  executable, gnutar, writeTextFile, lib, stdenv, ... }:

let
  str.stanzas = [{
    # we just use nix as an absolute path so we're not bundling any binaries
    type = "webapp";
    exec = "${exePkg}/bin/${executable}";
    host = "${domain}";
  }];
  configFile = writeTextFile {
    name = "keter.yml";
    text = (lib.generators.toYAML { } str);
  };

in stdenv.mkDerivation {
  name = lib.strings.sanitizeDerivationName "keter-bundle";
  buildCommand = ''
    mkdir -p config
    cp ${configFile} config/keter.yaml

    echo 'create a gzipped tarball'
    mkdir -p $out
    tar -zcvf $out/bundle.tar.gz.keter ./.
  '';
  buildInputs = [ gnutar ];
}
