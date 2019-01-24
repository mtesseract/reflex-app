{ system ? builtins.currentSystem
}:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
