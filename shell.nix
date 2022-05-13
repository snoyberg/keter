{ pkgs ? import ./nix/pkgs.nix, ... }:
pkgs.haskellPackages.shellFor {
  packages = ps : [ ps.keter ];
  # this is to build the examples from incoming
  extraDependencies = ps: {
    libraryHaskellDepends = [
      ps.yesod-websockets
      ps.yesod-core
      ps.stm-lifted
    ];
 };
  buildInputs = [
        pkgs.cabal-install
        ];
}
