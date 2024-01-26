{ pkgs, config, lib, inputs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  inherit (pkgs) anime4k;
  inherit (inputs) ssimSuperRes ssimDownscaler krigBilateral;
  fsrcnnx = pkgs.callPackage ./fsrcnnx.nix { inherit inputs; };
  cfg = config.my.home.mpv;
in
{
  options.my.home.mpv = {
    enable = mkEnableOption "mpv options";
  };

  config = mkIf cfg.enable {
    programs.mpv = {
      enable = true;
      # https://iamscum.wordpress.com/guides/videoplayback-guide/mpv-conf/#auto-profiles
      config = {
        # UI
        keep-open = "yes";

        # Video
        # profile = "high-quality";
        # vo = "gpu-next";
        # gpu-api = "vulkan";
        # hwdec = "auto-safe";

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
      };
      bindings =
        let
          setShader = { files, message }: ''no-osd change-list glsl-shaders set "${builtins.concatStringsSep ":" files}"; show-text "${message}"'';
        in
        {
          # A for originally blurry lines, B for pretty sharp ones, C for potato resolution
          "CTRL+1" = setShader {
            files = [
              "${anime4k}/Anime4K_Clamp_Highlights.glsl"
              "${anime4k}/Anime4K_Restore_CNN_VL.glsl"
              "${anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
              "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
            ];
            message = "Anime4K: Mode A (HQ)";
          };
          "CTRL+2" = setShader {
            files = [
              "${anime4k}/Anime4K_Clamp_Highlights.glsl"
              "${anime4k}/Anime4K_Restore_CNN_Soft_VL.glsl"
              "${anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
              "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
            ];
            message = "Anime4K: Mode B (HQ)";
          };
          "CTRL+3" = setShader {
            files = [
              "${anime4k}/Anime4K_Clamp_Highlights.glsl"
              "${anime4k}/Anime4K_Upscale_Denoise_CNN_x2_VL.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
              "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
            ];
            message = "Anime4K: Mode C (HQ)";
          };
          "CTRL+4" = setShader {
            files = [
              "${anime4k}/Anime4K_Clamp_Highlights.glsl"
              "${anime4k}/Anime4K_Restore_CNN_VL.glsl"
              "${anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
              "${anime4k}/Anime4K_Restore_CNN_M.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
              "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
            ];
            message = "Anime4K: Mode A+A (HQ)";
          };
          "CTRL+5" = setShader {
            files = [
              "${anime4k}/Anime4K_Clamp_Highlights.glsl"
              "${anime4k}/Anime4K_Restore_CNN_Soft_VL.glsl"
              "${anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
              "${anime4k}/Anime4K_Restore_CNN_Soft_M.glsl"
              "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
            ];
            message = "Anime4K: Mode B+B (HQ)";
          };
          "CTRL+6" = setShader {
            files = [
              "${anime4k}/Anime4K_Clamp_Highlights.glsl"
              "${anime4k}/Anime4K_Upscale_Denoise_CNN_x2_VL.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
              "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
              "${anime4k}/Anime4K_Restore_CNN_M.glsl"
              "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
            ];
            message = "Anime4K: Mode C+A (HQ)";
          };
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
        };
    };
  };
}
