{
  description = "NixOS Flake for pope";

  nixConfig = {
    experimental-features = [ "nix-command" "flakes" ];
    substituters = [
      "https://cache.nixos.org"
      "https://hyprland.cachix.org"
    ];

    # nix community's cache server
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://nixpkgs-wayland.cachix.org"
    ];

    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-23.05";

    home-manager = {
      # The stable URL
      # url = "github:nix-community/home-manager/release-23.05";
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nix language server, used by vscode & neovim
    nil.url = "github:oxalica/nil/2023-08-09";

    hyprland.url = "github:hyprwm/Hyprland";

    # Real-time audio
    musnix  = { url = "github:musnix/musnix"; };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, musnix, nixos-generators, ... }: {
    nixosConfigurations = {
      "soundwave" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = inputs;
        modules = [
          musnix.nixosModules.musnix
          ./hosts/soundwave

          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.pope = import ./hosts/soundwave/home.nix;

            home-manager.extraSpecialArgs = { inherit inputs; };
          }
        ];
      };
      "nixos-testing" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          ./hosts/nixos-testing

          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.pope = import ./hosts/nixos-testing/home.nix;
          }
        ];
      };
      "raspberrypi" = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";

        modules = [
          ./hosts/raspberrypi

          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.pi = import ./hosts/raspberrypi/home.nix;
          }

          # With this, we can build an SD card for the PI.
          # nix build .#nixosConfigurations.raspberrypi.config.formats.sd-aarch64
          nixos-generators.nixosModules.all-formats
        ];
      };
    };
  };
}
