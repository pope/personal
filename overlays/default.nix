{ self }:

# Where overrides go:
# - Overlay (`overlays/default.nix`): Global patches, build flags, and wrapper
#   fixes that should always apply everywhere (e.g., darktable, digikam,
#   gamescope).
# - Modules (`modules/`): Overrides that depend on module `cfg` options (e.g.,
#   steam, digikam OpenCL), theme/font variants, or `withCpuTuning` on core
#   packages like `mesa`, `pipewire`, and `emacs`. Overriding those in the
#   overlay would force a rebuild of everything in Nixpkgs that depends on them,
#   so we only tune the final `.package` in the module.
# - Packages (`packages/`): Custom scripts/fonts, side-by-side builds
#   (`darktable-nightly`), or packages where I manage the src/hash directly
#   (`renoise-mine`).

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

    # Exported so modules can tune `.package` options (like `mesa` or `pipewire`)
    # without overriding them in the overlay and rebuilding the world.
    withCpuTuning =
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

    withEmacsCpuTuning =
      pkg:
      if prev.stdenv.hostPlatform.isx86_64 && prev.stdenv.hostPlatform.isLinux then
        pkg.overrideAttrs (oldAttrs: {
          NIX_CFLAGS_COMPILE = "-march=x86-64-v3 -mtune=generic -O3";
          env = (oldAttrs.env or { }) // {
            BYTE_COMPILE_EXTRA_FLAGS = ''
              --eval '(setq native-comp-speed 3)' \
              --eval '(setq native-comp-driver-options '("-march=x86-64-v3" "-mtune=generic" "-O3"))'
            '';
          };
        })
      else
        pkg;
  in
  packages
  // (self.inputs.peng.overlays.default final prev)
  // {
    inherit stable withCpuTuning withEmacsCpuTuning;

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
        // {
          soy-ts-mode = self.inputs.tree-sitter-soy.packages.${final.stdenv.hostPlatform.system}.emacs;
        }
      );

    darktable =
      let
        darktableBase = (withCpuTuning (prev.darktable.override { withAi = true; })).overrideAttrs (
          oldAttrs:
          {
            nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ final.jq ];

            postPatch = (oldAttrs.postPatch or "") + ''
              jq '(.noiseprofiles[].models) |= . + [ .[] | select(.model == "M11") | .model = "M11-D" ]' \
                data/noiseprofiles.json > data/noiseprofiles.json.tmp
              mv data/noiseprofiles.json.tmp data/noiseprofiles.json
            '';
          }
          // prev.lib.optionalAttrs (prev.stdenv.hostPlatform.isx86_64 && prev.stdenv.hostPlatform.isLinux) {
            cmakeFlags =
              (prev.lib.filter (flag: !(prev.lib.hasPrefix "-DRAWSPEED_MARCH=" flag)) (
                oldAttrs.cmakeFlags or [ ]
              ))
              ++ [ "-DRAWSPEED_MARCH=-march=x86-64-v3" ];
          }
        );
      in
      final.symlinkJoin {
        name = "darktable-${darktableBase.version}";
        paths = [ darktableBase ];
        buildInputs = [ final.makeWrapper ];
        postBuild = ''
          for bin in darktable darktable-cli; do
            if [ -e "$out/bin/$bin" ]; then
              wrapProgram "$out/bin/$bin" \
                --add-flags "--conf plugins/ai/ort_library_path=${final.onnxruntime}/lib/libonnxruntime.so"
            fi
          done

          for desktopFile in "$out/share/applications"/*.desktop; do
            if [ -f "$desktopFile" ]; then
              target=$(readlink -f "$desktopFile")
              rm "$desktopFile"
              sed \
                -e "s|${darktableBase}/bin/darktable|$out/bin/darktable|g" \
                "$target" > "$desktopFile"
            fi
          done
        '';
        passthru = (darktableBase.passthru or { }) // {
          inherit (darktableBase) meta pname version;
          unwrapped = darktableBase;
        };
      };

    digikam = withCpuTuning (
      prev.digikam.overrideAttrs (oldAttrs: {
        patches = (oldAttrs.patches or [ ]) ++ [
          ./patches/digikam-loadingcache-lookup-perf.patch
        ];
      })
    );

    # TODO(pope): Remove after PR #587533 lands in nixpkgs/nixos-unstable
    glances = prev.glances.overridePythonAttrs (oldAttrs: rec {
      version = "4.5.9";
      src = prev.fetchFromGitHub {
        owner = "nicolargo";
        repo = "glances";
        tag = "v${version}";
        hash = "sha256-CvT9GQ29xXTVxC7V2nGmfDhHLsAtF61+XMMLG8cmHww=";
      };
      dependencies = (prev.lib.filter (p: (p.pname or "") != "python-jose") oldAttrs.dependencies) ++ [
        prev.python3Packages.pyjwt
      ];
      nativeCheckInputs = (oldAttrs.nativeCheckInputs or [ ]) ++ [
        prev.python3Packages.pysmart
      ];
    });

    jpeg-archive = withCpuTuning prev.jpeg-archive;

    mozjpeg = withCpuTuning prev.mozjpeg;

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
    gamescope = withCpuTuning prev.gamescope;

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
