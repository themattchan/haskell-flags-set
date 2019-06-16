{ mkDerivation, base, ghc-prim, stdenv, vector }:
mkDerivation {
  pname = "flags-set";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ghc-prim vector ];
  license = stdenv.lib.licenses.asl20;
}
