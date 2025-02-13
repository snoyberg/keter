{ config, lib, pkgs, ... }:

let
  cfg = config.services.keter-ng;

in
{
  options.services.keter-ng = {
    enable = lib.mkEnableOption (lib.mdDoc ''
      keter — a web app deployment manager.
    '');

    root = lib.mkOption {
      type = lib.types.str;
      default = "/opt/keter";
      description = lib.mdDoc "Mutable state folder for keter";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.haskellPackages.keter;
      defaultText = lib.literalExpression "pkgs.haskellPackages.keter";
      description = lib.mdDoc "The keter package to be used";
    };

    globalKeterConfig = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = lib.mdDoc ''
        A custom YAML configuration for Keter. This content will be directly
        written to the `keter-config.yml` file without modification. See
        <https://github.com/snoyberg/keter/blob/master/etc/keter-config.yaml>
        for reference.
      '';
    };
  };

  config = lib.mkIf cfg.enable (
    let
      incoming = "${cfg.root}/incoming";

      globalKeterConfigFile = pkgs.writeTextFile {
        name = "keter-config.yml";
        text = cfg.globalKeterConfig;
      };
    in
    {
      systemd.services.keter-ng = {
        description = "keter app loader";
        script = ''
          set -xe
          mkdir -p ${incoming}
          ${lib.getExe cfg.package} ${globalKeterConfigFile};
        '';
        wantedBy = [ "multi-user.target" "nginx.service" ];

        serviceConfig = {
          Restart = "always";
          RestartSec = "10s";
        };

        after = [
          "network.target"
          "local-fs.target"
          "postgresql.service"
        ];
      };
    }
  );
}

