{ testers, self }:

testers.nixosTest {
  name = "vm-test";

  nodes.server = { pkgs, ... }: {

    environment = {
      etc = {

        "app1.sh" = {
          text = ''
            #!/bin/sh
            while true; do
              ${pkgs.netcat}/bin/nc -l "$PORT" <<'EOF'
            HTTP/1.1 200 OK
            Content-Length: 17
            Connection: close

            I am version one.
            EOF
            done
          '';
          mode = "0755";
        };

        "app2.sh" = {
          text = ''
            #!/bin/sh
            while true; do
              ${pkgs.netcat}/bin/nc -l "$PORT" <<'EOF'
            HTTP/1.1 200 OK
            Content-Length: 17
            Connection: close

            I am version two.
            EOF
            done
          '';
          mode = "0755";
        };

        "broken.sh" = {
          text = ''
            #!/bin/sh
            exit 1
          '';
          mode = "0755";
        };

        "keter.yaml".text = ''
          stanzas:
            - type: webapp
              exec: ../app.sh
              hosts:
                - localhost
        '';

        "deploy_app1.sh" = {
          text = ''
            rm -rf /tmp/bundle /tmp/app.keter
            mkdir -p /tmp/bundle/config
            cp /etc/app1.sh /tmp/bundle/app.sh
            cp /etc/keter.yaml /tmp/bundle/config/keter.yaml
            tar -C /tmp/bundle -czvf /tmp/app.keter .
            cp /tmp/app.keter /opt/keter/incoming
          '';
          mode = "0755";
        };

        "deploy_app2.sh" = {
          text = ''
            rm -rf /tmp/bundle /tmp/app.keter
            mkdir -p /tmp/bundle/config
            cp /etc/app2.sh /tmp/bundle/app.sh
            cp /etc/keter.yaml /tmp/bundle/config/keter.yaml
            tar -C /tmp/bundle -czvf /tmp/app.keter .
            cp /tmp/app.keter /opt/keter/incoming
          '';
          mode = "0755";
        };

        "deploy_broken.sh" = {
          text = ''
            rm -rf /tmp/bundle /tmp/app.keter
            mkdir -p /tmp/bundle/config
            cp /etc/broken.sh /tmp/bundle/app.sh
            cp /etc/keter.yaml /tmp/bundle/config/keter.yaml
            tar -C /tmp/bundle -czvf /tmp/app.keter .
            cp /tmp/app.keter /opt/keter/incoming
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
    server.succeed(". /etc/deploy_app1.sh")
    server.sleep(10)
    server.succeed("curl localhost | grep 'I am version one.'")

    server.succeed(". /etc/deploy_broken.sh")
    server.sleep(10)
    server.succeed("curl localhost | grep 'I am version one.'")

    server.succeed(". /etc/deploy_app2.sh")
    server.sleep(120)
    server.succeed("curl localhost | grep 'I am version two.'")
  '';
}
