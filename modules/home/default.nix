{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:

let
  imports = map (p: ./. + "/${p}") (
    builtins.filter (p: p != "default.nix") (builtins.attrNames (builtins.readDir ./.))
  );
in
{
  imports = [
    inputs.nix-colors.homeManagerModules.default
  ]
  ++ imports;

  config.nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    package = lib.mkDefault pkgs.nix;
    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      nixpkgs-unstable.flake = inputs.nixpkgs-unstable;
    };
    settings = {
      auto-optimise-store = true;
      builders-use-substitutes = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      extra-substituters = [
        "https://nix-community.cachix.org"
        "https://nixpkgs-wayland.cachix.org"
      ];
      extra-trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      ];
      trusted-users = [ config.home.username ];
    };
  };
}
