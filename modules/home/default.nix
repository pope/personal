{
  inputs,
  config,
  pkgs,
  lib,
  osConfig ? null,
  ...
}:

let
  imports = map (p: ./. + "/${p}") (
    builtins.filter (p: p != "default.nix") (builtins.attrNames (builtins.readDir ./.))
  );
in
{
  inherit imports;

  config.nix = lib.mkIf (osConfig == null) {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    package = lib.mkDefault pkgs.nix;
    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      nixpkgs-stable.flake = inputs.nixpkgs-stable;
    };
    settings = {
      auto-optimise-store = false;
      builders-use-substitutes = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      extra-substituters = [
        "https://nix-community.cachix.org"
      ];
      extra-trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      trusted-users = [ config.home.username ];
    };
  };
}
