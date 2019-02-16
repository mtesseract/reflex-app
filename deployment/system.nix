{ modulesPath, ... }:

let
  backend = ((import ../.) { system = "x86_64-linux"; }).ghc.backend;
  frontend = import ../frontend-browser { system = "x86_64-linux"; };
  networkConf = import ./network.nix;
  dnsConf = import ./dns.nix;
  sshKeys = import ./ssh.nix;
in
{
  imports =
    [ "${modulesPath}/profiles/qemu-guest.nix"
    ];

  system.stateVersion = "18.09";
  time.timeZone = "Europe/Berlin";
  boot = {
    initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "ehci_pci" "virtio_pci" "sr_mod" "virtio_blk" ];
    kernelModules = [ "kvm-amd" ];
    loader = {
      grub = {
        enable = true;
        device = "/dev/vda";
      };
    };
  };
  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };
  swapDevices = [ { device = "/dev/disk/by-label/swap"; } ];

  networking = networkConf;

  services = {
    ntp.enable = true;
    openssh.enable = true;
    nginx = {
      enable = true;
      virtualHosts = {
        "${dnsConf.frontend}" = {
          enableACME = true;
          forceSSL = true;
          root = "${frontend}/bin/frontend.jsexe";
          extraConfig = ''
            gzip_static on;
            gzip_proxied expired no-cache no-store private auth;
          '';
        };
        "${dnsConf.backend}" = {
          enableACME = true;
          forceSSL = true;
          locations = {
            "/" = {
              proxyPass = "http://localhost:8080";
              extraConfig = ''
                add_header Access-Control-Allow-Origin *;
              '';
            };
          };
        };
      };
    };
  };
  systemd.services.backend = {
    description = "Reflex-App backend";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    environment = {
      DB_HOST = "db";
      DB_USERNAME = "app";
      DB_PASSWORD = "app";
      DB_DATABASE = "app";
      JWK = ''
        {
          "alg": "RS256",
          "kty": "RSA",
          "use": "sig",
          "x5c": [
            "MIIDCTCCAfGgAwIBAgIJEJmiRXdUiCMeMA0GCSqGSIb3DQEBCwUAMCIxIDAeBgNVBAMTF210ZXNzZXJhY3QuZXUuYXV0aDAuY29tMB4XDTE5MDIwODEzMjMwMVoXDTMyMTAxNzEzMjMwMVowIjEgMB4GA1UEAxMXbXRlc3NlcmFjdC5ldS5hdXRoMC5jb20wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDbIPpxVk8S6YR1vNxLKeTdTqi4ZpK6RIzVqBL6cjrggqZ06pDjX7VM5jMPkLThIquWbYRTrybGuOglSGnsPUczZ/WQa+Trb7lkESqeBOoqDzPmeFupkfpTNVOT/g1EnnAPvJGgob4kD1p5IBxOnHQCUhsnsdHpPwhUTd1vi3R+0PIdWHRNDgTZLm2GmmKmqnzQTx7wRK7QXiCnNlz1DKZdohCeJVCCf7l19dR4fN8TGlom6vpPar5bsGpP592xnJRAKvOjUyf2Box1O7ivk8jjN6s8BMtXPwUK18OFsN0I+yppojDdGHgZtZq1Q4Zt30g2Lr24pi48xOB2Cnazm2dzAgMBAAGjQjBAMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYEFCeKoKNTr+GHPLpJfQUf5NslKwghMA4GA1UdDwEB/wQEAwIChDANBgkqhkiG9w0BAQsFAAOCAQEAAeDZ+GG5Ih0ZFA1oxt/liIpBvwEAUhvSVGuef6DmPKg+aMIT305sbAdXXAU7isOGZ92VU6MVN0800ZhG0+GRp1kmpchRLBYWGJESlXe/QEUXeWsbgbDoJGO5BGnOz7+dnYE8M5eNHWlm/k22K+E/KBm2WkOokC3Yd5ioUfEoabdy8nwUHVW0bmPRYe/dFIPqiNPMnR2+u3moEELj0G60X4OjIWiwO8w4ECmZ9TUlu0cuSwepX6HfkllKzNEgS4a2k83ofLGHSnlpvrREXadjvyMIRHMkZ8vh2ymu5vm40kfiCA3+6QJfFgyU06YeyF4hFfa+XChSKWA3Bkj/VK19Lw=="
          ],
          "n": "2yD6cVZPEumEdbzcSynk3U6ouGaSukSM1agS-nI64IKmdOqQ41-1TOYzD5C04SKrlm2EU68mxrjoJUhp7D1HM2f1kGvk62-5ZBEqngTqKg8z5nhbqZH6UzVTk_4NRJ5wD7yRoKG-JA9aeSAcTpx0AlIbJ7HR6T8IVE3db4t0ftDyHVh0TQ4E2S5thppipqp80E8e8ESu0F4gpzZc9QymXaIQniVQgn-5dfXUeHzfExpaJur6T2q-W7BqT-fdsZyUQCrzo1Mn9gaMdTu4r5PI4zerPATLVz8FCtfDhbDdCPsqaaIw3Rh4GbWatUOGbd9INi69uKYuPMTgdgp2s5tncw",
          "e": "AQAB",
          "kid": "QjM4N0YxQzk0RURFOEIwQzU0MjM0RUYyNTZGQjIzNTYzQTk0OEYxQQ",
        }
      '';
    };
    serviceConfig = {
      ExecStart = "${backend}/bin/backend";
    };
  };

  users.users.root.openssh.authorizedKeys.keys = sshKeys;
  environment.noXlibs = true;
}
