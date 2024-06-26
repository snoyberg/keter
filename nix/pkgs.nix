import ./pin.nix {
  config = {

    packageOverrides = pkgs:
      let lib = pkgs.haskell.lib;
      in
      {

        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
        packageOverrides = hpNew: hpOld: {
                keter = hpNew.callPackage ../default.nix {};
                stm-lifted = lib.doJailbreak (lib.markUnbroken hpOld.stm-lifted);
                };
        };
    };
  };
}
