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
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-24.05";
    nixpkgs-2305.url = "github:NixOS/nixpkgs/release-23.05";

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
    flake-utils.url = "github:numtide/flake-utils";
    nix-colors.url = "github:misterio77/nix-colors";
    nix-formatter-pack = {
      url = "github:Gerschtli/nix-formatter-pack";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    # nix language server, used by vscode & neovim
    nil = {
      url = "github:oxalica/nil/2023-08-09";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
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
    keymapp = {
      url = "github:pope/keymapp-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    fingerprint-sensor = {
      url = "github:ahbnr/nixos-06cb-009a-fingerprint-sensor";
      inputs.nixpkgs.follows = "nixpkgs";
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
    , nixpkgs-2305
    , nixgl
    , nixvim
    , keymapp
    , fingerprint-sensor
    , ...
    } @ inputs:
    let
      eachSystem = nixpkgs.lib.genAttrs [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      mkNixosSystem =
        { name
        , system
        , extraModules ? [ ]
        , user ? "pope"
        }: {
          inherit name;
          value = nixpkgs.lib.nixosSystem {
            inherit system;
            specialArgs = {
              inherit inputs self;
              pkgs-stable = import nixpkgs-stable {
                inherit system;
                config.allowUnfree = true;
              };
              pkgs-2305 = import nixpkgs-2305 {
                inherit system;
                config.allowUnfree = true;
                config.permittedInsecurePackages = [
                  "python-2.7.18.6"
                  "python-2.7.18.6-env"
                ];
              };
            };
            modules = extraModules ++ [
              (_: {
                nixpkgs.overlays = [
                  keymapp.overlays.default
                  self.overlays.default
                ];
              })
              (./hosts + "/${name}")
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.extraSpecialArgs = {
                  inherit inputs self;
                  pkgs-stable = import nixpkgs-stable {
                    inherit system;
                    config.allowUnfree = true;
                  };
                };
                home-manager.backupFileExtension = "hm-backup";

                home-manager.users.${user} =
                  import (./hosts + "/${name}/home.nix");
              }
            ];
          };
        };
      mkHomeManagerConfig =
        { name
        , system
        , extraOverlays ? [ ]
        }:
        let
          inherit (nixpkgs.lib) last;
          inherit (nixpkgs.lib.strings) toLower splitString;
          hostname = toLower (last (splitString "@" name));
        in
        {
          inherit name;
          value = home-manager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
              overlays = extraOverlays ++ [ self.overlays.default ];
            };
            extraSpecialArgs = { inherit inputs self; };
            modules = [
              (./hosts + "/${hostname}/home.nix")
            ];
          };
        };
    in
    {
      nixosConfigurations = builtins.listToAttrs [
        (mkNixosSystem {
          name = "soundwave";
          system = "x86_64-linux";
          extraModules = [
            nixos-hardware.nixosModules.common-cpu-amd
            nixos-hardware.nixosModules.common-cpu-amd-pstate
            nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
            nixos-hardware.nixosModules.common-pc
            nixos-hardware.nixosModules.common-pc-ssd
            hyprland.nixosModules.default
            musnix.nixosModules.musnix
          ];
        })
        (mkNixosSystem {
          name = "ravage";
          system = "x86_64-linux";
          extraModules = [
            fingerprint-sensor.nixosModules.open-fprintd
            fingerprint-sensor.nixosModules.python-validity
            hyprland.nixosModules.default
            musnix.nixosModules.musnix
            nixos-hardware.nixosModules.lenovo-thinkpad-t480
          ];
        })
        (mkNixosSystem {
          name = "rumble";
          system = "x86_64-linux";
          extraModules = [
            hyprland.nixosModules.default
            musnix.nixosModules.musnix
            nixos-hardware.nixosModules.framework-13-7040-amd
          ];
        })
        (mkNixosSystem {
          name = "nixos-testing";
          system = "x86_64-linux";
          extraModules = [
            hyprland.nixosModules.default
          ];
        })
        (mkNixosSystem {
          name = "raspberrypi";
          system = "aarch64-linux";
          user = "pi";
          extraModules = [
            # With this, we can build an SD card for the PI.
            # nix build .#nixosConfigurations.raspberrypi.config.formats.sd-aarch64
            nixos-generators.nixosModules.all-formats
          ];
        })
      ];
      homeConfigurations = builtins.listToAttrs [
        (mkHomeManagerConfig {
          name = "pope@Death-Star";
          system = "x86_64-linux";
          extraOverlays = [ nixgl.overlay ];
        })
        (mkHomeManagerConfig {
          name = "deck@poopdeck";
          system = "x86_64-linux";
        })
        (mkHomeManagerConfig {
          name = "pope@galvatron";
          system = "aarch64-darwin";
        })
      ];
      nixosModules.default = { ... }: { imports = [ ./modules/nixos ]; };
      homeManagerModules.default = import ./modules/home self;
      packages = eachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pkgs-stable = nixpkgs-stable.legacyPackages.${system};
          nixvim' = nixvim.legacyPackages.${system};
          nixvimModule = {
            inherit pkgs;
            module = import ./modules/nixvim;
            # You can use `extraSpecialArgs` to pass additional arguments to your module files
            extraSpecialArgs = { inherit pkgs-stable; };
          };
          nvim = nixvim'.makeNixvimWithModule nixvimModule;
          mypkgs = import ./packages { inherit pkgs; } // { inherit nvim; };
        in
        mypkgs);
      overlays.default = import ./overlays { inherit self; };
      devShells = eachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          updatePackages = pkgs.writeShellScriptBin "updatePackages" ''
            cd packages
            ${pkgs.nvfetcher}/bin/nvfetcher
          '';
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [ nvfetcher updatePackages ];
          };
        });
      checks = eachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pkgs-stable = nixpkgs-stable.legacyPackages.${system};
          nixvimLib = nixvim.lib.${system};
          nixvimModule = {
            inherit pkgs;
            module = import ./modules/nixvim;
            extraSpecialArgs = { inherit pkgs-stable; };
          };
        in
        {
          nixvim = nixvimLib.check.mkTestDerivationFromNixvimModule nixvimModule;
        });
      formatter = eachSystem (system:
        nix-formatter-pack.lib.mkFormatter {
          pkgs = nixpkgs.legacyPackages.${system};
          config.tools = {
            deadnix.enable = true;
            nixpkgs-fmt.enable = true;
            statix.enable = true;
          };
        });
    };
}
