import ./pin.nix {
  config = {

    packageOverrides = pkgs: {

        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
        packageOverrides = hpNew: hpOld: {
                keter = hpNew.callPackage ../default.nix {};

                http-reverse-proxy = hpNew.callHackageDirect {
                    pkg = "http-reverse-proxy";
                    ver = "0.6.0.1";
                    sha256 = "09z9swznhzxb97ns8hnyjssm91ngsi4bvlqy6bmphqhj9c1m345x";
                } {};
                };
        };
    };
  };
}
