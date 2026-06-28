{ self }:

(
  _final: prev:
  let
    inherit ((import ../lib/umport.nix { inherit (prev) lib; })) umport;

    mypkgs = self.packages.${prev.stdenv.hostPlatform.system};

    stable = import self.inputs.nixpkgs-stable {
      inherit (prev.stdenv.hostPlatform) system;
      config.allowUnfree = true;
    };

    znver4 = import self.inputs.nixpkgs {
      config = {
        allowUnfree = true;
        rocmSupport = true;
      };
      localSystem = {
        gcc.arch = "znver4";
        gcc.tune = "znver4";
        system = "x86_64-linux";
      };
    };

    skylake = import self.inputs.nixpkgs {
      localSystem = {
        gcc.arch = "skylake";
        gcc.tune = "skylake";
        system = "x86_64-linux";
      };
    };

    names = map (f: prev.lib.strings.removeSuffix ".nix" (baseNameOf f)) (umport {
      path = ../packages;
      exclude = [
        ../packages/default.nix
      ];
    });
    packages = builtins.listToAttrs (
      map (name: {
        inherit name;
        value = mypkgs.${name};
      }) names
    );
  in
  packages
  // {
    inherit stable skylake znver4;

    emacsPackagesFor =
      emacs:
      (prev.emacsPackagesFor emacs).overrideScope (
        efinal: _eprev: {
          odin-ts-mode = efinal.callPackage ../packages/emacs/odin-ts-mode.nix { };
        }
      );

    darktable = mypkgs.darktable.override { withAi = true; };

    # TODO(pope): Remove this override after the NDI updater script runs
    obs-studio-plugins = prev.obs-studio-plugins // {
      distroav = prev.obs-studio-plugins.distroav.override {
        ndi-6 = prev.ndi-6.overrideAttrs (_: {
          src = prev.fetchurl {
            url = "https://downloads.ndi.tv/SDK/NDI_SDK_Linux/Install_NDI_SDK_v6_Linux.tar.gz";
            hash = "sha256-8DFPJFRG3vxIi2POtGiazxqWWu79ray3BXG7IWqMwYM=";
          };
        });
      };
    };

    renoise-mine = prev.renoise.override (
      let
        version = "354";
        releasePath =
          if prev.stdenv.hostPlatform.system == "x86_64-linux" then
            (prev.fetchurl rec {
              name = "rns_${version}_linux_x86_64.tar.gz";
              url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/software/${name}";
              hash = "sha256-k94/d+syBSOaCy5DWRljc1TzCzRTxPaNlJD4uE5vTFM=";
            })
          else
            (prev.fetchurl rec {
              name = "rns_${version}_linux_arm64.tar.gz";
              url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/software/${name}";
              hash = "sha256-ylykdTTW65+SCExuLBhT0gW0BfgqEuhaz3c+krWzbwc=";
            });
      in
      {
        inherit releasePath;
      }
    );
  }
  // prev.lib.optionalAttrs prev.stdenv.isDarwin {

    ctpv =
      (prev.ctpv.override {
        inherit (prev.llvmPackages_16) stdenv;
      }).overrideAttrs
        (oldAttrs: {
          meta.platforms = oldAttrs.meta.platforms ++ [ "aarch64-darwin" ];
        });

  }
)
