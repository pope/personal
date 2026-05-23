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
        ../packages/_sources
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

    # TODO(pope): Remove when https://github.com/NixOS/nixpkgs/issues/513245
    # is resolved.
    openldap = prev.openldap.overrideAttrs (_: {
      doCheck = !prev.stdenv.hostPlatform.isi686;
    });

    # TODO(pope): Remove when https://github.com/NixOS/nixpkgs/issues/523200
    # is resolved.
    inherit (stable) bubblewrap;

    # TODO(pope): Remove this when hashes are updated
    _1password-gui = prev._1password-gui.overrideAttrs (_: {
      src = prev.fetchurl {
        url = "https://downloads.1password.com/linux/tar/stable/x86_64/1password-8.12.21.x64.tar.gz";
        hash = "sha256-JwiMi2iozP6jWSIUtgXla86aSAhuUob7snqtUbeXPpI=";
      };
    });

    # TODO(pope): Remove when jedi is updated
    python313Packages = prev.python313Packages // {
      inherit (stable.python313Packages) pyls-isort python-lsp-server;
    };
    vscode-extensions = prev.vscode-extensions // {
      ms-python = prev.vscode-extensions.ms-python // {
        inherit (stable.vscode-extensions.ms-python) python;
      };
    };

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
        version = "352";
        releasePath =
          if prev.stdenv.hostPlatform.system == "x86_64-linux" then
            (prev.requireFile rec {
              name = "rns_${version}_linux_x86_64.tar.gz";
              url = "file:///media/cyberia/nix-files/software/${name}";
              sha256 = "14ncyi3pzsbl5hy3gdrq62rk6r17mmxl7arnwxapaywz62j6gh2c";
            })
          else
            (prev.requireFile rec {
              name = "rns_${version}_linux_arm64.tar.gz";
              url = "file:///media/cyberia/nix-files/software/${name}";
              sha256 = "19jzjvnhic1nndj7xdphl5r79pc3qjbkjrxm0rbl0zjp8s73ici8";
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
