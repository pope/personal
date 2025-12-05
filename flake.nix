{
  description = "NixOS Flake for pope";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nix-colors.url = "github:misterio77/nix-colors";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixvim = {
      url = "github:nix-community/nixvim/nixos-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mac-app-util = {
      url = "github:hraban/mac-app-util";
      inputs = {
        # See https://github.com/hraban/mac-app-util/issues/39
        cl-nix-lite.url = "github:r4v3n6101/cl-nix-lite/url-fix";
        nixpkgs.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt-nix";
      };
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
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      home-manager,
      mac-app-util,
      nixgl,
      nixpkgs,
      nixvim,
      self,
      treefmt-nix,
      ...
    }@inputs:
    let
      eachSystem = nixpkgs.lib.genAttrs [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      treefmtEval =
        system:
        treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} (_: {
          projectRootFile = "flake.nix";
          programs = {
            deadnix.enable = true;
            mdformat.enable = true;
            nixfmt.enable = true;
            statix.enable = true;
          };
        });
      mkNixosSystem =
        {
          name,
          system,
          user ? "pope",
        }:
        {
          inherit name;
          value = nixpkgs.lib.nixosSystem {
            inherit system;
            specialArgs = { inherit inputs self; };
            modules = [
              (./hosts + "/${name}")
              home-manager.nixosModules.home-manager
              { imports = [ inputs.sops-nix.nixosModules.sops ]; }
              {
                home-manager = {
                  backupFileExtension = "hm-backup";
                  extraSpecialArgs = { inherit inputs self; };
                  sharedModules = [
                    inputs.sops-nix.homeManagerModules.sops
                  ];
                  useGlobalPkgs = true;
                  useUserPackages = true;
                  users.${user} = import (./hosts + "/${name}/home.nix");
                };
              }
            ];
          };
        };
      mkHomeManagerConfig =
        {
          name,
          system,
          extraOverlays ? [ ],
          extraModules ? [ ],
          hostnameOverride ? null,
        }:
        let
          inherit (nixpkgs.lib) last;
          inherit (nixpkgs.lib.strings) toLower splitString;
          hostname =
            if hostnameOverride != null then hostnameOverride else toLower (last (splitString "@" name));
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
              { imports = [ inputs.sops-nix.homeManagerModules.sops ]; }
              (./hosts + "/${hostname}/home.nix")
            ]
            ++ extraModules;
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
          name = "pope@galvatron";
          system = "aarch64-darwin";
          extraModules = [ mac-app-util.homeManagerModules.default ];
        })
        (mkHomeManagerConfig {
          name = "deck";
          hostnameOverride = "steamdeck";
          system = "x86_64-linux";
        })
      ];
      nixosModules.default = _: { imports = [ ./modules/nixos ]; };
      homeManagerModules.default = _: { imports = [ ./modules/home ]; };
      packages = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          nixvim' = nixvim.legacyPackages.${system};
          nixvimModule = {
            inherit pkgs;
            module = import ./modules/nixvim;
          };
          nvim = nixvim'.makeNixvimWithModule nixvimModule;
          mypkgs = import ./packages { inherit pkgs; } // {
            inherit nvim;
          };
        in
        mypkgs
      );
      overlays.default = import ./overlays { inherit self; };
      devShells = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          mypkgs = self.packages.${system};
          treefmt = (treefmtEval system).config.build.wrapper;
        in
        {
          default = pkgs.mkShell {
            packages =
              with pkgs;
              with mypkgs;
              [
                add-files-to-nix-store
                backup-git-repos
                deadnix
                nixfmt-rfc-style
                nixos-rebuild-remote
                nvfetcher
                statix
                treefmt
                update-my-packages
                wake-up-soundwave
                wake-up-unicron
              ];
          };
        }
      );
      checks = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          nixvimLib = nixvim.lib.${system};
          nixvimModule = {
            inherit pkgs;
            module = import ./modules/nixvim;
          };
        in
        {
          formatting = (treefmtEval system).config.build.check self;
          nixvim = nixvimLib.check.mkTestDerivationFromNixvimModule nixvimModule;
        }
      );
      formatter = eachSystem (system: (treefmtEval system).config.build.wrapper);
    };
}
