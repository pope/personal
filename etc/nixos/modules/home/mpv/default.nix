{ pkgs, pkgs-stable, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkMerge mkOption optionalAttrs;
  inherit (pkgs) anime4k modernx fsrcnnx;
  krigBilateral = "${pkgs.krigBilateral}/KrigBilateral.glsl";
  ssimDownscaler = "${pkgs.ssimDownscaler}/SSimDownscaler.glsl";
  ssimSuperRes = "${pkgs.ssimSuperRes}/SSimSuperRes.glsl";
  setShader = { files, message }: ''no-osd change-list glsl-shaders set "${builtins.concatStringsSep ":" files}"; show-text "${message}"'';
  anime4khqbindings = import ./anime4k-hq-bindings.nix { inherit anime4k setShader; };
  anime4kfastbindings = import ./anime4k-fast-bindings.nix { inherit anime4k setShader; };
  cfg = config.my.home.mpv;
in
{
  options.my.home.mpv = {
    enable = mkEnableOption "mpv options";
    enableHqAnimeSettings = mkEnableOption "use the HQ Anime4K shaders";
    enableVulkan = mkOption {
      default = pkgs.stdenv.isLinux;
      example = true;
      description = "Whether to enable Vulkan GPU API.";
      type = lib.types.bool;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      modernx
      streamlink
      yt-dlp
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
        vo = if pkgs.stdenv.isLinux then "gpu-next" else "libmpv";
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
      } // optionalAttrs cfg.enableVulkan {
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

      profiles = {
        crt-guest-advanced-ntsc = {
          glsl-shaders = "${./shaders/crt-guest-advanced-ntsc.glsl}";
        };
        crt-lottes = {
          glsl-shaders = "${./shaders/crt-lottes.glsl}";
        };
        gba = {
          glsl-shaders = "${./shaders/gba.glsl}";
          scale = "nearest";
        };
      };
    } // optionalAttrs pkgs.stdenv.isLinux {
      scripts = with pkgs.mpvScripts; [
        modernx
        mpris
        thumbfast
        visualizer
      ];
    } // optionalAttrs pkgs.stdenv.isDarwin {
      # Using the stable pkgs for Darwin. Using these, Swift doesn't have to be
      # recompiled and everything downloads quickly.
      package = pkgs-stable.wrapMpv pkgs-stable.mpv-unwrapped {
        scripts = with pkgs-stable.mpvScripts; [
          modernx
          thumbfast
          visualizer
        ];
      };
    };
  };
}
