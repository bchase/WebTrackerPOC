let
  pkgs = import <nixpkgs> { };
in
  { poc_project = pkgs.haskellPackages.callPackage ./default.nix { };
  }
