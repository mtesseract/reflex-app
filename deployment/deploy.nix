let snapshot = import ./pkgs.nix;
in import (builtins.fetchGit snapshot + "/nixos") {
  system = "x86_64-linux";
  configuration = { imports = [ ./system.nix ]; };
}
