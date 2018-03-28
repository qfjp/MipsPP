let
  pkgs = import <nixpkgs> {  };
in
  {
    mipp = pkgs.haskellPackages.callPackage ./default.nix {};
  }
