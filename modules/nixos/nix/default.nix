{ config, inputs, ... }:

{
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    registry = {
      nixpkgs.flake = inputs.nixpkgs;
    };
    settings = {
      auto-optimise-store = true;
      builders-use-substitutes = true;
      # Enable Flakes and the new command-line tool
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
      trusted-users = [ config.my.nixos.mainUser ];
    };
  };
}
