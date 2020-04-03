{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
  
let

  inherit (nixpkgs) pkgs;
  hp = import ./pkgs.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  ghcPkg = haskellPackages.ghcWithHoogle hp;

  nixDeps = with pkgs; [
    ghcPkg
    cabal-install
  ];

in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs = [
      nixDeps
    ];
  }
