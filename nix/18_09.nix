let
  fetchNixpkgs = import ./fetchNixpkgs.nix;
in fetchNixpkgs {
     rev          = "f5689d5d6b9be626e8528cb63e2f2cf8f9a0c87e";
     sha256       = "101f0nnk6i25x9nxviy4by1mpxbdplq81hssvvgi8x54yvh8m3rq";
   }
