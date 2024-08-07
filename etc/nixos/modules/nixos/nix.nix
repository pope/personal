{ config, inputs, ... }:

{
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    registry = {
      nixpkgs-stable.flake = inputs.nixpkgs-stable;
    };
    settings = {
      auto-optimise-store = true;
      builders-use-substitutes = true;
      # Enable Flakes and the new command-line tool
      experimental-features = [ "nix-command" "flakes" ];
      trusted-users = [ config.my.nixos.mainUser ];
    };
  };
}
