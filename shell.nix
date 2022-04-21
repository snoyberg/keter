{ nixpkgs ? import ./nix/pin.nix {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, array, async, attoparsec, base
      , blaze-builder, bytestring, case-insensitive, conduit
      , conduit-extra, containers, data-default, directory, filepath
      , fsnotify, hspec, http-client, http-conduit, http-reverse-proxy
      , http-types, HUnit, indexed-traversable, lib, lifted-base, mtl
      , network, optparse-applicative, process, random, regex-tdfa, stm
      , tar, template-haskell, text, time, tls, tls-session-manager
      , transformers, unix, unix-compat, unordered-containers, vector
      , wai, wai-app-static, wai-extra, warp, warp-tls, yaml, zlib
      , cabal-install, yesod, stm-lifted, yesod-websockets
      }:
      mkDerivation {
        pname = "keter";
        version = "1.5";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryToolDepends = [ cabal-install ];
        libraryHaskellDepends = [
          aeson array async attoparsec base blaze-builder bytestring
          case-insensitive conduit conduit-extra containers data-default
          directory filepath fsnotify http-client http-conduit
          http-reverse-proxy http-types indexed-traversable lifted-base mtl
          network optparse-applicative process random regex-tdfa stm tar
          template-haskell text time tls tls-session-manager transformers
          unix unix-compat unordered-containers vector wai wai-app-static
          wai-extra warp warp-tls yaml zlib yesod stm-lifted yesod-websockets
        ];
        executableHaskellDepends = [ base data-default filepath ];
        testHaskellDepends = [
          base bytestring conduit hspec HUnit transformers unix
        ];
        homepage = "http://www.yesodweb.com/";
        description = "Web application deployment manager, focusing on Haskell web frameworks";
        license = lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
