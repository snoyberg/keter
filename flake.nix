{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";
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
              portEnvVars = pkgs.callPackage ./vm-port-env-vars.nix { inherit self; };
            in
            {
              inherit (pkgs.haskellPackages) keter;
              inherit basic ipFromHeader portEnvVars;
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

            # Add keter-rate-limiting-plugin from Hackage
            keter-rate-limiting-plugin = (hprev.callHackageDirect
              {
                pkg = "keter-rate-limiting-plugin";
                ver = "0.2.0.2";
                sha256 = "sha256-ngoymeitp7dsjvCI4uIrFhY8+BdaCsYdMVxd5solb5M=";
              }
              { }).overrideAttrs (oldAttrs: {
                librarySystemDepends = (oldAttrs.librarySystemDepends or []) ++ [ prev.zlib ];
                buildInputs = (oldAttrs.buildInputs or []) ++ [ prev.zlib ];
              });

            # ram is not yet in nixpkgs, needed by crypton >= 1.1.0
            ram = hprev.callHackageDirect
              {
                pkg = "ram";
                ver = "0.21.1";
                sha256 = "sha256-J+gP+rZft1xkxzxmvXcktnDIymRkjg5u5wmhEge3+GQ=";
              }
              { };

            crypton =
              let
                minVersion = "1.1.0";
              in
              if prev.lib.versionAtLeast hprev.crypton.version minVersion then
                builtins.trace
                  "Note: nixpkgs already has crypton ${hprev.crypton.version} (>= ${minVersion}), override not needed"
                  hprev.crypton
              else
                prev.haskell.lib.overrideCabal hprev.crypton (drv: {
                  version = minVersion;
                  sha256 = "sha256-Pwxfg4fbg+crD0Bu1FPWB4I10VmHxAz+1mjwmKH0Pig=";
                  revision = null;
                  editedCabalFile = null;
                  libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ [
                    hprev.base16
                    hprev.ram
                  ];
                });

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
