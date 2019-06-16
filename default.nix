let

  config = { allowUnfree = true; };

  bruteForce = newPkgs: oldPkgs:
  {
    mkDerivation = args: oldPkgs.mkDerivation (args //
    {
      doCheck   = false;
      doHaddock = false;
      jailbreak = true;
    });
  };

  loadAllPackages = directory: pkgsNew:
    let
      paths = builtins.attrNames (builtins.readDir directory);

      toKeyVal = file:
      {
        name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

        value = pkgs.haskell.lib.dontCheck (pkgsNew.callPackage (directory + "/${file}") { });
      };

    in
      builtins.listToAttrs (map toKeyVal paths);

  overlays = [
    (newPkgs: oldPkgs: {

      haskellPackages = oldPkgs.haskellPackages.override {
      overrides = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {})
        [  bruteForce

           (haskellPackagesNew: haskellPackagesOld:
            let
                allPkgs = loadAllPackages ./nix haskellPackagesNew;
            in
                allPkgs //
                {
                  flags-set = pkgs.haskell.lib.doCheck (haskellPackagesNew.callPackage ./flags-set.nix { });
                }
           )
        ];
      };
    })
  ];

  nixpkgs = import ./nix/18_09.nix;

  pkgs = import nixpkgs { inherit config overlays; };

in
  { inherit (pkgs.haskellPackages) flags-set cabal cabal-install; }
