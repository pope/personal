{
  description = "NixOS Flake for pope";

  nixConfig = {
    experimental-features = [ "nix-command" "flakes" ];
    builders-use-substitutes = true;

    extra-substituters = [
      "https://hyprland.cachix.org"
      "https://nix-community.cachix.org"
      "https://nixpkgs-wayland.cachix.org"
      "https://anyrun.cachix.org"
    ];

    extra-trusted-public-keys = [
      "anyrun.cachix.org-1:pqBobmOjI7nKlsUMV25u9QHa9btJK65/C8vnO3p346s="
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

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nix-colors.url = "github:misterio77/nix-colors";

    nix-formatter-pack = {
      url = "github:Gerschtli/nix-formatter-pack";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nix language server, used by vscode & neovim
    nil = {
      url = "github:oxalica/nil/2023-08-09";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    anyrun = {
      url = "github:Kirottu/anyrun";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Real-time audio
    musnix = {
      url = "github:musnix/musnix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fish-rose-pine = {
      url = "github:rose-pine/fish";
      flake = false;
    };

    plow = {
      url = "github:six-ddc/plow/v1.3.1";
      flake = false;
    };

    keymapp = {
      url = "github:pope/keymapp-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ssimSuperRes = {
      url = "file+https://gist.githubusercontent.com/igv/2364ffa6e81540f29cb7ab4c9bc05b6b/raw/15d93440d0a24fc4b8770070be6a9fa2af6f200b/SSimSuperRes.glsl";
      flake = false;
    };
    ssimDownscaler = {
      url = "file+https://gist.githubusercontent.com/igv/36508af3ffc84410fe39761d6969be10/raw/575d13567bbe3caa778310bd3b2a4c516c445039/SSimDownscaler.glsl";
      flake = false;
    };
    krigBilateral = {
      url = "file+https://gist.githubusercontent.com/igv/a015fc885d5c22e6891820ad89555637/raw/038064821c5f768dfc6c00261535018d5932cdd5/KrigBilateral.glsl";
      flake = false;
    };

    fsrcnnx_lineart = {
      url = "file+https://github.com/igv/FSRCNN-TensorFlow/releases/download/1.1/checkpoints_params.7z";
      flake = false;
    };
    fsrcnnx8 = {
      url = "file+https://github.com/igv/FSRCNN-TensorFlow/releases/download/1.1/FSRCNNX_x2_8-0-4-1.glsl";
      flake = false;
    };
    fsrcnnx16 = {
      url = "file+https://github.com/igv/FSRCNN-TensorFlow/releases/download/1.1/FSRCNNX_x2_16-0-4-1.glsl";
      flake = false;
    };

    mpv_prescalers = {
      url = "github:bjin/mpv-prescalers";
      flake = false;
    };

    modernx = {
      url = "github:cyl0/ModernX/0.6.0";
      flake = false;
    };
  };

  outputs =
    { self
    , home-manager
    , hyprland
    , musnix
    , nix-formatter-pack
    , nixos-generators
    , nixos-hardware
    , nixpkgs
    , nixpkgs-stable
    , ...
    } @ inputs:
    let
      eachSystem = nixpkgs.lib.genAttrs [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
    in
    {
      nixosConfigurations = {
        "soundwave" = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = {
            inherit inputs;
            pkgs-stable = import nixpkgs-stable {
              inherit system;
              config.allowUnfree = true;
              config.permittedInsecurePackages = [
                "python-2.7.18.6"
                "python-2.7.18.6-env"
              ];
            };
          };
          modules = [
            musnix.nixosModules.musnix
            hyprland.nixosModules.default
            ./hosts/soundwave
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit inputs; };

              home-manager.users.pope = import ./hosts/soundwave/home.nix;
            }
          ];
        };
        "ravage" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-t480
            hyprland.nixosModules.default
            ./hosts/ravage
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit inputs; };

              home-manager.users.pope = import ./hosts/ravage/home.nix;
            }
          ];
        };
        "nixos-testing" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            ./hosts/nixos-testing
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit inputs; };

              home-manager.users.pope = import ./hosts/nixos-testing/home.nix;
            }
          ];
        };
        "raspberrypi" = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            ./hosts/raspberrypi
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit inputs; };

              home-manager.users.pi = import ./hosts/raspberrypi/home.nix;
            }

            # With this, we can build an SD card for the PI.
            # nix build .#nixosConfigurations.raspberrypi.config.formats.sd-aarch64
            nixos-generators.nixosModules.all-formats
          ];
        };
      };
      homeConfigurations = {
        "pope@Death-Star" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
          extraSpecialArgs = { inherit inputs; };
          modules = [
            ./hosts/death-star/home.nix
          ];
        };
        "pope@galvatron" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."aarch64-darwin";
          extraSpecialArgs = { inherit inputs; };
          modules = [
            ./hosts/galvatron/home.nix
          ];
        };
      };
      overlays = import ./overlays { inherit inputs; };
      homeManagerModules.default = import ./modules/home self;
      formatter = eachSystem (system:
        nix-formatter-pack.lib.mkFormatter {
          pkgs = nixpkgs.legacyPackages.${system};
          config.tools = {
            deadnix.enable = true;
            nixpkgs-fmt.enable = true;
            statix.enable = true;
          };
        }
      );
    };
}
