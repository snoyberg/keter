{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
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

          checks =
            let
              basic = pkgs.callPackage ./vm.nix { inherit self; };
              ipFromHeader = pkgs.callPackage ./vm-ip-from-header.nix { inherit self; };
            in
            {
              inherit (pkgs.haskellPackages) keter;
              inherit basic ipFromHeader;
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
          };
        }) // {
      overlays.default = _: prev: {
        haskell = prev.haskell // {
          # override for all compilers
          packageOverrides = prev.lib.composeExtensions prev.haskell.packageOverrides (_: hprev: {

            http-reverse-proxy =
              let
                minVersion = "0.6.2.0";
              in
              if prev.lib.versionAtLeast hprev.http-reverse-proxy.version minVersion then
                builtins.trace
                  "Note: nixpkgs already has http-reverse-proxy ${hprev.http-reverse-proxy.version} (>= ${minVersion}), override not needed"
                  hprev.http-reverse-proxy
              else
                hprev.callHackageDirect
                  {
                    pkg = "http-reverse-proxy";
                    ver = minVersion;
                    sha256 = "sha256-cknEOvB2t2Qcyv5yFKCEWFvC4gjkCU0k7AFAA4VQ3yA=";
                  }
                  { };

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
