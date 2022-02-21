# https://nixos.wiki/wiki/Module
{ config, pkgs, bash,  lib, ... }:
let
  cfg = config.services.keter;
  keterConfig = pkgs.callPackage ./config.nix {
    globalketerConfig = cfg.globalKeterConfig // { root = cfg.keterRoot; };
    keterRoot = cfg.keterRoot;
    bundleCfg = cfg.bundle;
    keter = cfg.keterPackage;
  };
in
{
  options.services.keter = {
    enable = lib.mkEnableOption "keter service";
    keterRoot = lib.mkOption {
      type = lib.types.str;
      default = "/opt/keter";
      description = ''Mutable state folder for keter. In here the bundles get extracted,
                      logs get written and the app's work folder is set to'';
    };
    keterPackage = lib.mkOption {
      type = lib.types.package;
      default = null;
      description = ''set the keter package to be used, usually pkgs.haskellPackages.keter'';
    };

    globalKeterConfig = lib.mkOption {
      type = lib.types.attrs;
      default = {
        ip-from-header = true;
        listeners = [{
            host = "*4";
            port = 81;
        }];
      };
      description = ''root is required to function in nixos .
                      port 81 is set to default to allow nginx to reverse proxy into keter,
                      so that nginx can do ssl management.
                      You want that ip-from-header in the nginx setup case
                      so it's not set to 127.0.0.1.
                      Keep in mind root is overwritten by keterRoot.
                      which is required for the nix derivation.
                      '';
    };

    bundle = {
        executable = lib.mkOption {
            type = lib.types.str;
            default = "bash";
            description = ''The executable to be run in the bin folder of the package'';
        };

        domain = lib.mkOption {
            type = lib.types.str;
            default = "example.com";
            description = ''The domain keter will bind to'';
        };

        # these won't work with standard nix options because they have
        # to be put in a tarball, so we use a runtime script to load settings.
        publicScript = lib.mkOption {
            type = lib.types.str;
            default = "";
            description = ''
                Allows loading of public environment variables,
                these are emitted to the log so it shouldn't contain secrets.
                For example:
                ```
                ADMIN_EMAIL=hi@example.com
                ```
            '';
        };

        secretScript = lib.mkOption {
            type = lib.types.str;
            default = "";
            description = ''
                Allows loading of private environment variables,
                for example:
                ```
                MY_AWS_KEY=$(cat /run/keys/AWS_ACCESS_KEY_ID)
                ```
                '';
        };

        # This indirection is required to ensure the nix path
        # gets copied over to the target machine in remote deployments.
        # Furthermore, it's important that we use exec to
        # run the binary otherwise we get process leakage.
        package = lib.mkOption {
            type = lib.types.package;
            default = bash;
            description = ''The package which will be copied over to the nixos configuration,
                            from which executable name runs.
                        '';
        };
    };

  };

  config = lib.mkIf cfg.enable {
    systemd.services.keter = keterConfig.keter;
    systemd.services.keter-load-bundle = keterConfig.load-keter-bundle;
  };
}
