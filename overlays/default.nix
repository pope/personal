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
  // (self.inputs.peng.overlays.default final prev)
  // {
    inherit stable;

    # TODO(pope): Remove after PR #576407 lands in nixpkgs/nixos-unstable.
    inherit (stable) handbrake;

    # TODO(pope): Remove once peng is updated for Zig 0.17.
    peng = (self.inputs.peng.overlays.default final prev).peng.override {
      zig = final.zig_0_16;
    };

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

    # TODO(pope): Remove after PRs #575818, #576469, and #577221 land in nixpkgs/nixos-unstable.
    linuxPackages_latest = prev.linuxPackages_latest.extend (
      _lfinal: lprev: {
        framework-laptop-kmod = lprev.framework-laptop-kmod.overrideAttrs (oldAttrs: {
          makeFlags = oldAttrs.makeFlags ++ [
            "INSTALL_MOD_PATH=${placeholder "out"}"
            "INSTALL_MOD_DIR=extra"
            "DEPMOD=true"
          ];
          installPhase = null;
          installTargets = [ "modules_install" ];
        });
        v4l2loopback = lprev.v4l2loopback.overrideAttrs (oldAttrs: {
          nativeBuildInputs = prev.lib.remove prev.kmod oldAttrs.nativeBuildInputs;
        });
        xpad-noone = lprev.xpad-noone.overrideAttrs (oldAttrs: {
          makeFlags = oldAttrs.makeFlags ++ [
            "KERNEL_SRC=${lprev.kernel.dev}/lib/modules/${lprev.kernel.modDirVersion}/build"
            "INSTALL_MOD_PATH=${placeholder "out"}"
            "INSTALL_MOD_DIR=kernel/drivers/xpad-noone"
          ];
          installPhase = null;
          installTargets = [ "modules_install" ];
          installFlags = [
            "-C"
            "${lprev.kernel.dev}/lib/modules/${lprev.kernel.modDirVersion}/build"
            "M=$(PWD)"
          ];
          strictDeps = true;
          __structuredAttrs = true;
        });
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
