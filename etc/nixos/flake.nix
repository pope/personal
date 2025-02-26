{
  description = "NixOS Flake for pope";

  nixConfig = {
    experimental-features = [ "nix-command" "flakes" ];
    builders-use-substitutes = true;

    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://nixpkgs-wayland.cachix.org"
    ];

    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-24.11";

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
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Real-time audio
    musnix = {
      url = "github:musnix/musnix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fingerprint-sensor = {
      url = "github:ahbnr/nixos-06cb-009a-fingerprint-sensor";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    wezterm = {
      url = "github:wez/wezterm?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , home-manager
    , nix-formatter-pack
    , nixpkgs
    , nixgl
    , nixvim
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
        , user ? "pope"
        }: {
          inherit name;
          value = nixpkgs.lib.nixosSystem {
            inherit system;
            specialArgs = { inherit inputs self; };
            modules = [
              (./hosts + "/${name}")
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.extraSpecialArgs = { inherit inputs self; };
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
              config = {
                allowUnfree = true;
                joypixels.acceptLicense = true;
              };
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
        })
        (mkNixosSystem {
          name = "ravage";
          system = "x86_64-linux";
        })
        (mkNixosSystem {
          name = "rumble";
          system = "x86_64-linux";
        })
        (mkNixosSystem {
          name = "unicron";
          system = "x86_64-linux";
        })
        (mkNixosSystem {
          name = "skrapnel";
          system = "x86_64-linux";
        })
        (mkNixosSystem {
          name = "nixos-testing";
          system = "x86_64-linux";
        })
        (mkNixosSystem {
          name = "raspberrypi";
          system = "aarch64-linux";
          user = "pi";
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
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
          };
          nixvim' = nixvim.legacyPackages.${system};
          nixvimModule = {
            inherit pkgs;
            module = import ./modules/nixvim;
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
            packages = with pkgs; [
              deadnix
              nixpkgs-fmt
              nvfetcher
              statix
              updatePackages
            ];
          };
        });
      checks = eachSystem (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
          };
          nixvimLib = nixvim.lib.${system};
          nixvimModule = {
            inherit pkgs;
            module = import ./modules/nixvim;
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
