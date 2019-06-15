let
  pkgs = import (import ./nix/18_09.nix) {};
  proj = import ./default.nix;
in
  pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs = proj.haskell-flags-set.env.nativeBuildInputs ++
                  [
#                    proj.cabal
                    proj.cabal-install
                  ];
  }
