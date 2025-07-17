{ testers, self }:

testers.nixosTest {
  name = "vm-test";

  nodes.server = { pkgs, ... }: {

    environment =
      let
        nc_sh = { signature ? "", bad ? false }: {
          text =
            let
              content = ''
                Hello, world!
                ${signature}
              '';
              length = builtins.stringLength content;
            in
            ''
              #!/bin/sh
              ${if bad then "exit 1" else ""}
              while true; do
                ${pkgs.netcat}/bin/nc -l "$PORT" <<'EOF'
              HTTP/1.1 200 OK
              Content-Length: ${builtins.toString length}
              Connection: close

              ${content}
              EOF
              done
            '';
          mode = "0755";
        };

        deploy_sh = { num ? "" }: {
          text = ''
            mkdir -p /tmp/bundle/config
            cp /etc/nc${num}.sh /tmp/bundle/nc.sh
            cp /etc/keter.yaml /tmp/bundle/config/keter.yaml
            tar -C /tmp/bundle -czvf /tmp/nc.keter .
            cp /tmp/nc.keter /opt/keter/incoming/
          '';
          mode = "0755";
        };
      in
      {
        etc = {
          "nc.sh" = nc_sh { signature = "Test1!"; };
          "nc2.sh" = nc_sh { signature = "Test2!"; bad = true; };
          "nc3.sh" = nc_sh { signature = "Test3!"; };

          "keter.yaml".text = ''
            stanzas:
              - type: webapp
                exec: ../nc.sh
                hosts:
                  - localhost
          '';

          "deploy.sh" = deploy_sh { };
          "deploy2.sh" = deploy_sh { num = "2"; };
          "deploy3.sh" = deploy_sh { num = "3"; };
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

    # Crash-looping bundles should not get reloading stuck
    # https://github.com/snoyberg/keter/issues/294
    server.succeed("curl localhost | grep 'Test1!'") # Previosly deployed script is OK
    server.succeed(". /etc/deploy2.sh") # Load the crashed script
    server.sleep(10)
    server.succeed("curl localhost | grep 'Test1!'") # bad script doesn't break the good one
    server.succeed(". /etc/deploy3.sh") # Load new good script
    server.sleep(10)
    server.succeed("curl localhost | grep 'Test3!'") # bad script is unloaded and doesn't stuck
  '';
}
