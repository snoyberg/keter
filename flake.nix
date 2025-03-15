{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              self.overlays.default
            ];
          };
          hl = pkgs.haskell.lib;

        in
        {
          packages = {
            inherit (pkgs.haskellPackages) keter;
            default = pkgs.haskellPackages.keter;
          };

          checks = {
            inherit (pkgs.haskellPackages) keter;

            integratedTests = pkgs.callPackage ./vm.nix { inherit self; };
          };

          devShells.default = pkgs.haskellPackages.shellFor {
            packages =
              let devPkgs = [ ];
              in p: [ (hl.addBuildDepends p.keter devPkgs) ];
            buildInputs = with pkgs.haskellPackages; [
              cabal-fmt
              cabal-install
              hlint
            ];
            inherit shellHook;
          };
        }) // {
      overlays.default = _: prev: {
        haskell = prev.haskell // {
          # override for all compilers
          packageOverrides = prev.lib.composeExtensions prev.haskell.packageOverrides (_: hprev: {

            tar = hprev.tar_0_6_3_0;

            keter =
              let
                haskellSourceFilter = prev.lib.sourceFilesBySuffices ./. [
                  ".cabal"
                  ".hs"
                  ".c"
                  "LICENSE"
                ];
              in
              hprev.callCabal2nix "keter" haskellSourceFilter { };

          });
        };
      };
    };
}
