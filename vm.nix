{ testers, self }:

testers.nixosTest {
  name = "vm-test";

  nodes.server = { ... }: {

    imports = [
      ./keter.nix
    ];

    nix.settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };

    services.keter-ng = {
      enable = true;
      package = self.packages.x86_64-linux.keter;
      globalKeterConfig = ''
        root: /opt/keter
        rotate-logs: false
        listeners:
          - host: "*4"
            port: 80
      '';
    };

  };

  testScript = ''
    server.start()
    server.wait_for_unit("keter-ng.service")
    server.wait_for_open_port(80)
    server.succeed("curl localhost")
  '';
}
