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

    withV3 =
      pkg:
      if prev.stdenv.hostPlatform.isx86 && prev.stdenv.hostPlatform.isLinux then
        pkg.overrideAttrs (
          oldAttrs:
          let
            flags = " -march=x86-64-v3 -mtune=generic";
          in
          if (oldAttrs ? env) && (oldAttrs.env ? NIX_CFLAGS_COMPILE) then
            {
              env = oldAttrs.env // {
                NIX_CFLAGS_COMPILE = "${oldAttrs.env.NIX_CFLAGS_COMPILE}${flags}";
              };
            }
          else
            {
              NIX_CFLAGS_COMPILE = "${oldAttrs.NIX_CFLAGS_COMPILE or ""}${flags}";
            }
        )
      else
        pkg;
  in
  packages
  // (self.inputs.peng.overlays.default final prev)
  // {
    inherit stable withV3;

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

    darktable = (withV3 (prev.darktable.override { withAi = true; })).overrideAttrs (
      oldAttrs:
      prev.lib.optionalAttrs (prev.stdenv.hostPlatform.isx86_64 && prev.stdenv.hostPlatform.isLinux) {
        cmakeFlags =
          (prev.lib.filter (flag: !(prev.lib.hasPrefix "-DRAWSPEED_MARCH=" flag)) (
            oldAttrs.cmakeFlags or [ ]
          ))
          ++ [ "-DRAWSPEED_MARCH=-march=x86-64-v3" ];
      }
    );

    digikam = withV3 (
      prev.digikam.overrideAttrs (oldAttrs: {
        patches = (oldAttrs.patches or [ ]) ++ [
          ./patches/digikam-loadingcache-lookup-perf.patch
        ];
      })
    );

    jpeg-archive = withV3 prev.jpeg-archive;

    mozjpeg = withV3 prev.mozjpeg;

    tree-sitter-grammars = prev.tree-sitter-grammars.overrideScope (
      _gfinal: _gprev: {
        inherit tree-sitter-soy;
      }
    );
    tree-sitter = prev.tree-sitter // {
      inherit (final.tree-sitter-grammars) allGrammars withPlugins;
    };
  }
  // prev.lib.optionalAttrs prev.stdenv.hostPlatform.isLinux {
    gamescope = withV3 prev.gamescope;

    ghostty =
      (self.inputs.ghostty.packages.${prev.stdenv.hostPlatform.system}.default.override {
        enableX11 = false;
      }).overrideAttrs
        (oldAttrs: {
          zigBuildFlags =
            (prev.lib.filter (flag: !(prev.lib.hasPrefix "-Dcpu=" flag)) oldAttrs.zigBuildFlags)
            ++ [
              "-Dcpu=x86_64_v3"
            ];
        });

    scx = prev.scx // {
      rustscheds =
        if prev.stdenv.hostPlatform.isx86_64 then
          prev.scx.rustscheds.overrideAttrs (oldAttrs: {
            env = oldAttrs.env // {
              RUSTFLAGS = "${oldAttrs.env.RUSTFLAGS} -C target-cpu=x86-64-v3";
            };
          })
        else
          prev.scx.rustscheds;
      full = prev.scx.full.override { inherit (final) scx; };
    };
  }
)
