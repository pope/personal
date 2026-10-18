{ self }:

(
  final: prev:
  let
    inherit ((import ../lib/umport.nix { inherit (prev) lib; })) umport;

    # Exclude Emacs packages from top-level pkgs since they are added to the
    # emacsPackagesFor scope below.
    packages = import ../packages {
      pkgs = final;
      inherit (prev) lib;
      includeEmacs = false;
    };

    tree-sitter-soy =
      self.inputs.tree-sitter-soy.packages.${final.stdenv.hostPlatform.system}.tree-sitter-soy;

    stable = import self.inputs.nixpkgs-stable {
      inherit (prev.stdenv.hostPlatform) system;
      config.allowUnfree = true;
    };

    emacsFiles = umport {
      path = ../packages/emacs;
    };
  in
  packages
  // {
    inherit stable;

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
