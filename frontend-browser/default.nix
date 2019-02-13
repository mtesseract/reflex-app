{ system ? builtins.currentSystem
, nixpkgs ? import <nixpkgs> {  }
}:
let
  frontendMin = import ../frontend-js-minified { inherit system; };
  stdenv = nixpkgs.stdenv;
in
  stdenv.mkDerivation {
      name = "frontend-browser";
      src = ./.;
      buildInputs = [ frontendMin ];
      builder = ./builder.sh;
      inherit frontendMin;
  }
