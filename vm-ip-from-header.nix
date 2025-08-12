{ testers, self }:

testers.nixosTest {
  name = "vm-test-ip-from-header";

  nodes.server = { pkgs, ... }: {
    environment =
      let
        nc_sh = {
          text =
            let
              body = "Hello, world!\n";
              len = builtins.stringLength body;
            in
            ''
              #!/bin/sh
              while true; do
                ${pkgs.netcat}/bin/nc -l "$PORT" <<'EOF'
              HTTP/1.1 200 OK
              Content-Length: ${builtins.toString len}
              Connection: close

              ${body}
              EOF
              done
            '';
          mode = "0755";
        };

        deploy_sh = {
          text = ''
            mkdir -p /tmp/bundle/config
            cp /etc/nc.sh /tmp/bundle/nc.sh
            cp /etc/keter.yaml /tmp/bundle/config/keter.yaml
            tar -C /tmp/bundle -czf /tmp/nc.keter .
            cp /tmp/nc.keter /opt/keter/incoming/
          '';
          mode = "0755";
        };
      in
      {
        etc = {
          "nc.sh" = nc_sh;

          "keter.yaml".text = ''
            stanzas:
              - type: webapp
                exec: ../nc.sh
                hosts:
                  - localhost
          '';

          "deploy.sh" = deploy_sh;

          "send-request.sh" = {
            text = ''
              #!/bin/sh
              # From this test case: https://github.com/snoyberg/keter/issues/312
              malformed="$(printf '\xbf\xf0\x9f\x92\xa1')"
              curl -fsS -H "X-Forwarded-For:$malformed" http://localhost/ | grep 'Hello, world!'
            '';
            mode = "0755";
          };
        };

        systemPackages = with pkgs; [ netcat curl ];
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
        ip-from-header: true
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

    # Deploy the single test app
    server.succeed(". /etc/deploy.sh")

    # Wait until the app is serving the expected body
    server.wait_until_succeeds("curl -fsS http://localhost/ | grep 'Hello, world!'")

    # Malformed header check
    server.succeed("/etc/send-request.sh")
  '';
}
