{ globalketerConfig, keterRoot, writeShellScriptBin,
  writeTextFile, lib, bundleCfg, callPackage, coreutils
  , keter, ...
}:

let
  incoming = "${keterRoot}/incoming";


  globalKeterConfigFile = writeTextFile {
    name = "keter-config.yml";
    text = (lib.generators.toYAML { } globalketerConfig);
  };

  # If things are expected to change often, put it in the bundle!
  bundle = callPackage ./bundle.nix
    (bundleCfg // { exePkg = executable; });

  executable = writeShellScriptBin "${bundleCfg.executable}" ''
    #!/usr/bin/env bash
    export PATH=$PATH:${lib.makeBinPath [ coreutils ]}
    set -e
    ${bundleCfg.secretScript}
    set -xe
    ${bundleCfg.publicScript}
    exec ${bundleCfg.package}/bin/${bundleCfg.executable}
  '';

in {
  # This is keter, which loads our app on deploy, and checks if it is healthy
  # be very careful putting anything in here that changes,
  # any change to the systemd unit can cause brief windows of downtime
  # because nixos has to restart the unit.
  keter = {
    description = "keter app loader";
    script = ''
      set -xe
      mkdir -p ${incoming}
      { tail -F /opt/keter/log/keter/current.log -n 0 & ${keter}/bin/keter ${globalKeterConfigFile}; }
    '';
    wantedBy = [ "multi-user.target" "nginx.service" ];

    serviceConfig = {
      Restart="always";
      RestartSec="10s";
    };

    after = [
      "network.target"
      "local-fs.target"
      "postgresql.service"
      "redis.service"
    ];
  };

  # On deploy this will load our app, by moving it into the incoming dir
  # If the bundle content changes, this will run again.
  # Because the bundle content contains the nix path to the exectuable,
  # we inherit nix based cache busting.
  load-keter-bundle = {
    description = "load keter bundle into incoming folder";
    after = [ "keter.service" ];
    wantedBy = [ "multi-user.target" ];
    # we can't override keter bundles because it'll stop the previous app
    # https://github.com/snoyberg/keter#deploying
    script = ''
      set -xe
      cp ${bundle}/bundle.tar.gz.keter ${incoming}/supercede.keter
    '';
    path = [
      executable
    ]; # this is a hack to get the executable copied over to the machine.
  };
}
