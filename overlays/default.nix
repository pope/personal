{ self }:

(
  final: prev:
  let
    inherit ((import ../lib/umport.nix { inherit (prev) lib; })) umport;

    mypkgs = self.packages.${prev.stdenv.hostPlatform.system};

    tree-sitter-soy =
      self.inputs.tree-sitter-soy.packages.${final.stdenv.hostPlatform.system}.tree-sitter-soy;

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

    emacsFiles = umport {
      path = ../packages/emacs;
    };
  in
  packages
  // {
    inherit stable skylake znver4;

    emacsPackagesFor =
      emacs:
      (prev.emacsPackagesFor emacs).overrideScope (
        efinal: _eprev:
        builtins.listToAttrs (
          map (f: {
            name = prev.lib.strings.removeSuffix ".nix" (baseNameOf f);
            value = efinal.callPackage f { };
          }) emacsFiles
        )
      );

    darktable = prev.darktable.override { withAi = true; };

    digikam = prev.digikam.overrideAttrs (oldAttrs: {
      patches = (oldAttrs.patches or [ ]) ++ [
        ./patches/digikam-loadingcache-lookup-perf.patch
      ];
    });

    # TODO(pope): Remove after PR #573942 lands in nixpkgs/nixos-unstable
    # Fixes lix 2.95.4 meson configure error with unknown option "mimalloc"
    lix = prev.lix.overrideAttrs (oldAttrs: {
      mesonFlags = prev.lib.remove (prev.lib.mesonEnable "mimalloc" false) (oldAttrs.mesonFlags or [ ]);
    });

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

    tree-sitter-grammars = prev.tree-sitter-grammars.overrideScope (
      _gfinal: _gprev: {
        inherit tree-sitter-soy;
      }
    );
    tree-sitter = prev.tree-sitter // {
      inherit (final.tree-sitter-grammars) allGrammars withPlugins;
    };
  }
)
