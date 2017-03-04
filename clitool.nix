let
  pkgs = import <nixpkgs> { };
in
  {
  clitool = pkgs.haskellPackages.callPackage ./default.nix { } ;
  }
