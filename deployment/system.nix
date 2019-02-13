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
    };
    serviceConfig = {
      ExecStart = "${backend}/bin/backend";
    };
  };

  users.users.root.openssh.authorizedKeys.keys = sshKeys;
  environment.noXlibs = true;
}
