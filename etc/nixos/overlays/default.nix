{ self }:

(
  _final: prev:
  let
    inherit ((import ../lib/umport.nix { inherit (prev) lib; })) umport;

    mypkgs = self.packages.${prev.system};

    stable = import self.inputs.nixpkgs-stable {
      inherit (prev) system;
      config.allowUnfree = true;
    };

    names = builtins.map (f: prev.lib.strings.removeSuffix ".nix" (builtins.baseNameOf f)) (umport {
      path = ../packages;
      exclude = [
        ../packages/default.nix
        ../packages/_sources
      ];
    });
    packages = builtins.listToAttrs (
      builtins.map (name: {
        inherit name;
        value = mypkgs.${name};
      }) names
    );
  in
  packages
  // {
    inherit stable;

    renoise350 = prev.renoise.override (
      let
        version = "352";
        releasePath =
          if prev.system == "x86_64-linux" then
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

    inherit (stable) deadbeef;
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
