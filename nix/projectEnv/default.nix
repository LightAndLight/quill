let
  pkgs = import ./nixpkgs.nix;
in
  {
    inherit pkgs;
    haskell = pkgs.haskell.packages.ghc865;
    stdenv = pkgs.stdenv;
  }