{ system ? builtins.currentSystem
, nixpkgs ? import <nixpkgs> { inherit system; }
}:
let
  frontend = ((import ../.) { inherit system; }).ghcjs.frontend;
  pkgs = nixpkgs.pkgs;
  stdenv = nixpkgs.stdenv;
  closurecompiler = pkgs.closurecompiler;
  zopfli = pkgs.zopfli;
in
  stdenv.mkDerivation {
      name = "frontend-js-minified";
      buildInputs = [ frontend ];
      builder = ./builder.sh;
      inherit frontend;
      inherit closurecompiler;
      inherit zopfli;
  }
