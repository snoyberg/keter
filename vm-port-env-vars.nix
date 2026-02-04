{ testers, self }:

testers.nixosTest {
  name = "vm-test-port-env-vars";

  nodes.server = { pkgs, ... }: {
    environment =
      let
        # This app deliberately uses YESOD_PORT instead of PORT.
        # It will only work if keter's port-env-vars sets YESOD_PORT.
        nc_sh = {
          text =
            let
              body = "Hello from YESOD_PORT!\n";
              len = builtins.stringLength body;
            in
            ''
              #!/bin/sh
              while true; do
                ${pkgs.netcat}/bin/nc -l "$YESOD_PORT" <<'EOF'
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
                port-env-vars:
                  - YESOD_PORT
          '';

          "deploy.sh" = deploy_sh;
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

    # Deploy the app that listens on YESOD_PORT (not PORT)
    server.succeed(". /etc/deploy.sh")

    # The app only binds to YESOD_PORT, so this proves port-env-vars works
    server.wait_until_succeeds("curl -fsS http://localhost/ | grep 'Hello from YESOD_PORT!'")
  '';
}
