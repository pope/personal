{ pkgs, config, lib, inputs, ... }:

let
  inherit (lib) mkIf mkEnableOption mkMerge optionalAttrs optionals;
  inherit (inputs) ssimSuperRes ssimDownscaler krigBilateral;
  inherit (pkgs) anime4k;
  fsrcnnx = pkgs.callPackage ./fsrcnnx.nix { inherit inputs; };
  modernx = pkgs.callPackage ./modernx.nix { inherit inputs; };
  setShader = { files, message }: ''no-osd change-list glsl-shaders set "${builtins.concatStringsSep ":" files}"; show-text "${message}"'';
  anime4khqbindings = import ./anime4k-hq-bindings.nix { inherit anime4k setShader; };
  anime4kfastbindings = import ./anime4k-fast-bindings.nix { inherit anime4k setShader; };
  cfg = config.my.home.mpv;
in
{
  options.my.home.mpv = {
    enable = mkEnableOption "mpv options";
    enableHqAnimeSettings = mkEnableOption "use the HQ Anime4K shaders";
  };

  config = mkIf cfg.enable {
    home.packages = [
      modernx
    ];

    fonts.fontconfig.enable = true;

    programs.mpv = {
      enable = true;

      # https://iamscum.wordpress.com/guides/videoplayback-guide/mpv-conf/#auto-profiles
      config = {
        # UI
        keep-open = "yes";
        osc = "no";
        border = "no";

        # Video
        profile = "gpu-hq";
        vo = if pkgs.stdenv.isLinux then "gpu" else "libmpv";
        hwdec = "auto-safe";

        # Shaders
        glsl-shaders = builtins.concatStringsSep ":" [
          ssimSuperRes
          ssimDownscaler
          krigBilateral
        ];
        scale = "ewa_lanczossharp";
        dscale = "lanczos";
        cscale = "spline36";
        linear-downscaling = "no";
        correct-downscaling = "yes";
      } // optionalAttrs pkgs.stdenv.isLinux {
        gpu-api = "vulkan";
      };

      bindings = mkMerge [
        {
          "CTRL+7" = setShader {
            files = [ ssimSuperRes krigBilateral ];
            message = "SuperRes";
          };
          "CTRL+8" = setShader {
            files = [ "${fsrcnnx}/FSRCNNX_x2_8-0-4-1_LineArt.glsl" krigBilateral ];
            message = "FSRCNNX 8 LineArt";
          };
          "CTRL+9" = setShader {
            files = [ "${fsrcnnx}/FSRCNNX_x2_16-0-4-1.glsl" krigBilateral ];
            message = "FSRCNNX 16";
          };

          "CTRL+0" = ''no-osd change-list glsl-shaders clr ""; show-text "GLSL shaders cleared"'';
        }
        (mkIf cfg.enableHqAnimeSettings anime4khqbindings)
        (mkIf (!cfg.enableHqAnimeSettings) anime4kfastbindings)
      ];

      scripts = [
        modernx
        pkgs.mpvScripts.thumbfast
      ] ++ optionals pkgs.stdenv.isLinux [
        pkgs.mpvScripts.mpris
      ];
    };
  };
}
