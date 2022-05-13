{ pkgs ? import ./nix/pkgs.nix, ... }:
let
  ignore = import (builtins.fetchGit {
                     url = "https://github.com/hercules-ci/gitignore.nix";
                     rev = "bff2832ec341cf30acb3a4d3e2e7f1f7b590116a";
    }) { inherit (pkgs) lib; };
in
pkgs.haskellPackages.callCabal2nix "keter" (ignore.gitignoreSource ./.) { }
