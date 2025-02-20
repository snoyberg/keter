{ testers, self }:

testers.nixosTest {
  name = "vm-test";

  nodes.server = { pkgs, ... }: {

    environment = {
      etc = {

        "nc.sh" = {
          text = ''
            #!/bin/sh
            while true; do
              ${pkgs.netcat}/bin/nc -l "$PORT" <<'EOF'
            HTTP/1.1 200 OK
            Content-Length: 13
            Connection: close

            Hello, world!
            EOF
            done
          '';
          mode = "0755";
        };

        "keter.yaml".text = ''
          stanzas:
            - type: webapp
              exec: ../nc.sh
              hosts:
                - localhost
        '';

        "deploy.sh" = {
          text = ''
            mkdir -p /tmp/bundle/config
            cp /etc/nc.sh /tmp/bundle/nc.sh
            cp /etc/keter.yaml /tmp/bundle/config/keter.yaml
            tar -C /tmp/bundle -czvf /tmp/nc.keter .
            cp /tmp/nc.keter /opt/keter/incoming
          '';
          mode = "0755";
        };

      };

      systemPackages = [ pkgs.netcat ];
    };

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
        rotate-logs: true
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
    server.succeed(". /etc/deploy.sh")
    server.sleep(10)
    server.succeed("curl localhost | grep 'Hello, world!'")
    server.succeed("! grep -q 'file is locked' /opt/keter/log/keter.log")
    server.succeed("touch /opt/keter/incoming/nc.keter")
    server.sleep(10)
    server.succeed("! grep -q 'file is locked' /opt/keter/log/keter.log")
  '';
}
