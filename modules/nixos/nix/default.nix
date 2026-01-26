{
  config,
  inputs,
  lib,
  ...
}:

let
  cfg = config.my.nixos.nix;
in
{
  options.my.nixos.nix = {
    enable = lib.mkEnableOption "nix config options";
    enableBinaryCacheSubstitutor = lib.mkEnableOption "my binary cache server";
  };

  config = lib.mkIf cfg.enable {
    nix = {
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 7d";
      };
      nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
      registry = {
        nixpkgs.flake = inputs.nixpkgs;
        nixpkgs-unstable.flake = inputs.nixpkgs-unstable;
      };
      settings = {
        auto-optimise-store = true;
        builders-use-substitutes = true;
        # Enable Flakes and the new command-line tool
        experimental-features = [
          "nix-command"
          "flakes"
        ];
        substituters =
          lib.optionals cfg.enableBinaryCacheSubstitutor [
            "http://skrapnel:5000"
          ]
          ++ [
            "https://nix-community.cachix.org"
          ];
        trusted-public-keys =
          lib.optionals cfg.enableBinaryCacheSubstitutor [
            "skrapnel:vM+AMNsRUqjN3EYBHiUvy384DZfQr57Y9drFFGmPLiM="
          ]
          ++ [
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          ];
        trusted-users = [ config.my.nixos.mainUser ];
      };
    };
  };
}
