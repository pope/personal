{ pkgs }:
let
  getDefaultShader =
    x: "${pkgs.mpv-shim-default-shaders}/share/mpv-shim-default-shaders/shaders/${x}";
  mkProfileDef =
    {
      name,
      shortcut,
      desc,
      settingGroups ? [ ],
      settings ? { },
    }:
    let
      unmerged = [ defaultConfigValues ] ++ settingGroups ++ [ settings ];
      shaders = builtins.concatLists (builtins.catAttrs "shaders" unmerged);
      glsl-shaders = builtins.concatStringsSep ":" shaders;
      merged = builtins.foldl' (a: b: a // b) { } unmerged;
    in
    {
      inherit name desc shortcut;
      profile = {
        profile-desc = desc;
        inherit (merged)
          cscale
          dscale
          linear-downscaling
          scale
          ;
        inherit glsl-shaders;
      };
    };

  defaultConfigValues = {
    cscale = "spline64";
    dscale = "mitchell";
    linear-downscaling = true;
    scale = "ewa_lanczos";
    shaders = [ ];
  };
  staticGrainDefault = {
    shaders = [
      (getDefaultShader "noise_static_luma.hook")
      (getDefaultShader "noise_static_chroma.hook")
    ];
  };
  fsrcnnxHigh = {
    scale = "ewa_lanczossharp";
    shaders = [
      (getDefaultShader "FSRCNNX_x2_16-0-4-1.glsl")
    ];
  };
  fsrcnnx = {
    scale = "ewa_lanczossharp";
    shaders = [
      (getDefaultShader "FSRCNNX_x2_8-0-4-1.glsl")
    ];
  };
  ssimDownscaler = {
    dscale = "mitchell";
    linear-downscaling = false;
    shaders = [
      (getDefaultShader "SSimDownscaler.glsl")
    ];
  };
  krigBilateral = {
    cscale = "spline64";
    shaders = [
      (getDefaultShader "KrigBilateral.glsl")
    ];
  };
in
{
  generic = mkProfileDef {
    name = "generic";
    shortcut = "g-g";
    desc = "FSRCNNX";
    settingGroups = [
      fsrcnnx
      ssimDownscaler
      krigBilateral
      staticGrainDefault
    ];
  };

  genericHigh = mkProfileDef {
    name = "generic-high";
    desc = "FSRCNNX x16";
    shortcut = "g-G";
    settingGroups = [
      fsrcnnxHigh
      ssimDownscaler
      krigBilateral
      staticGrainDefault
    ];
  };

  fsr = mkProfileDef {
    name = "fsr";
    desc = "FRS";
    shortcut = "CTRL+9";
    settings.shaders = [
      (getDefaultShader "FSR.glsl")
      (getDefaultShader "CAS-scaled.glsl")
    ];
  };

  nvscaler = mkProfileDef {
    name = "nvscaler";
    desc = "NVidia Scaler";
    shortcut = "CTRL+8";
    settings.shaders = [
      (getDefaultShader "NVScaler.glsl")
    ];
  };

  # CRT

  crtGuestAdvancedNtsc = mkProfileDef {
    name = "crt-guest-advanced-ntsc";
    desc = "CRT (Guest Advanced NTSC)";
    shortcut = "g-V";
    settingGroups = [
      krigBilateral
      staticGrainDefault
    ];
    settings.shaders = [
      "${./shaders/crt-guest-advanced-ntsc.glsl}"
    ];
  };

  crtLottes = mkProfileDef {
    name = "crt-lottes";
    desc = "CRT (Lottes)";
    shortcut = "g-C";
    settingGroups = [
      krigBilateral
      staticGrainDefault
    ];
    settings.shaders = [
      "${./shaders/crt-lottes.glsl}"
    ];
  };

  # Animation

  artcnn = mkProfileDef {
    name = "artcnn";
    desc = "ArtCNN";
    shortcut = "CTRL+7";
    settings.shaders = [
      "${pkgs.artcnn}/share/artcnn/GLSL/ArtCNN_C4F16_DS.glsl"
    ];
  };

  # Anime 4k HQ

  anime4kAHq = mkProfileDef {
    name = "anime4k-a";
    desc = "Anime4K: Mode A (HQ)";
    shortcut = "CTRL+1";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_VL.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
  };

  anime4kBHq = mkProfileDef {
    name = "anime4k-b";
    desc = "Anime4K: Mode B (HQ)";
    shortcut = "CTRL+2";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_Soft_VL.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
  };

  anime4kCHq = mkProfileDef {
    name = "anime4k-c";
    desc = "Anime4K: Mode C (HQ)";
    shortcut = "CTRL+3";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_Denoise_CNN_x2_VL.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
  };

  anime4kAAHq = mkProfileDef {
    name = "anime4k-a-a";
    desc = "Anime4K: Mode A+A (HQ)";
    shortcut = "CTRL+4";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_VL.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_M.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
  };

  anime4kBBHq = mkProfileDef {
    name = "anime4k-b-b";
    desc = "Anime4K: Mode B+B (HQ)";
    shortcut = "CTRL+5";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_Soft_VL.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_Soft_M.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
  };

  anime4kCAHq = mkProfileDef {
    name = "anime4k-c-a";
    desc = "Anime4K: Mode C+A (HQ)";
    shortcut = "CTRL+6";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_Denoise_CNN_x2_VL.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_M.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
  };

  # Anime 4k Fast

  anime4kAFast = mkProfileDef {
    name = "anime4k-a";
    desc = "Anime4K: Mode A (Fast)";
    shortcut = "CTRL+1";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_M.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
  };

  anime4kBFast = mkProfileDef {
    name = "anime4k-b";
    desc = "Anime4K: Mode B (Fast)";
    shortcut = "CTRL+2";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_Soft_M.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
  };

  anime4kCFast = mkProfileDef {
    name = "anime4k-c";
    desc = "Anime4K: Mode C (Fast)";
    shortcut = "CTRL+3";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_Denoise_CNN_x2_M.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
  };

  anime4kAAFast = mkProfileDef {
    name = "anime4k-a-a";
    desc = "Anime4K: Mode A+A (Fast)";
    shortcut = "CTRL+4";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_M.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_S.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
  };

  anime4kBBFast = mkProfileDef {
    name = "anime4k-b-b";
    desc = "Anime4K: Mode B+B (Fast)";
    shortcut = "CTRL+5";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_Soft_M.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_Soft_S.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
  };

  anime4kCAFast = mkProfileDef {
    name = "anime4k-c-a";
    desc = "Anime4K: Mode C+A (Fast)";
    shortcut = "CTRL+6";
    settings.shaders = [
      "${pkgs.anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_Denoise_CNN_x2_M.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${pkgs.anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${pkgs.anime4k}/Anime4K_Restore_CNN_S.glsl"
      "${pkgs.anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
  };
}
