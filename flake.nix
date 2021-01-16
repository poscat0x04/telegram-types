{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = telegram-types-dev.envFunc { withHoogle = true; };
            defaultPackage = telegram-types;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          telegram-types = hpkgs.callCabal2nix "telegram-types" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit telegram-types;
            telegram-types-dev = addBuildTools telegram-types [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
