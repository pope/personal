{ pkgs, enableHqSettings }:
let
  getDefaultShader =
    x: "${pkgs.mpv-shim-default-shaders}/share/mpv-shim-default-shaders/shaders/${x}";
  getRetroShader = x: "${pkgs.mpv-retro-shaders}/share/mpv-retro-shaders/GLSL/${x}";
  mkProfileDef' =
    {
      name,
      shortcut,
      desc,
      baseSettings,
      settingGroups ? [ ],
      settings ? { },
    }:
    let
      unmerged = [ baseSettings ] ++ settingGroups ++ [ settings ];
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
          cscale-antiring
          dscale
          linear-downscaling
          scale
          scale-antiring
          ;
        inherit glsl-shaders;
      };
    };
  mkHqProfileDef = args: mkProfileDef' (args // { baseSettings = hqBaseSettings; });
  mkFastProfileDef = args: mkProfileDef' (args // { baseSettings = fastBaseSettings; });
  mkProfileDef = args: if enableHqSettings then mkHqProfileDef args else mkFastProfileDef args;

  fastBaseSettings = {
    # Scaling
    cscale = "bilinear";
    cscale-antiring = 0.6;
    dscale = "bilinear";
    linear-downscaling = true;
    scale = "bilinear";
    scale-antiring = 0.6;
    # Shaders
    shaders = [ ];
  };
  hqBaseSettings = {
    # Scaling
    cscale = "ewa_lanczossharp";
    cscale-antiring = 0.6;
    dscale = "mitchell";
    linear-downscaling = true;
    scale = "ewa_lanczossharp";
    scale-antiring = 0.6;
    # Shaders
    shaders = [ ];
  };

  staticGrainDefault = {
    shaders = [
      (getDefaultShader "noise_static_luma.hook")
      (getDefaultShader "noise_static_chroma.hook")
    ];
  };
  fsrcnnxHigh = {
    shaders = [
      (getDefaultShader "FSRCNNX_x2_16-0-4-1.glsl")
    ];
  };
  fsrcnnx = {
    shaders = [
      (getDefaultShader "FSRCNNX_x2_8-0-4-1.glsl")
    ];
  };
  krigBilateral = {
    shaders = [
      (getDefaultShader "KrigBilateral.glsl")
    ];
  };
in
{
  myFast = mkFastProfileDef {
    name = "my-fast";
    shortcut = "CTRL+0";
    desc = "Fast";
  };

  generic = mkFastProfileDef {
    name = "generic";
    shortcut = "g-g";
    desc = "FSRCNNX";
    settingGroups = [
      fsrcnnx
      krigBilateral
      staticGrainDefault
    ];
  };

  genericHigh = mkHqProfileDef {
    name = "generic-high";
    desc = "FSRCNNX x16";
    shortcut = "g-G";
    settingGroups = [
      fsrcnnxHigh
      krigBilateral
      staticGrainDefault
    ];
  };

  fsr = mkProfileDef {
    name = "fsr";
    desc = "FSR";
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
    name = "crt-hyllian";
    desc = "CRT (Hyllian)";
    shortcut = "g-V";
    settingGroups = [
      krigBilateral
      staticGrainDefault
    ];
    settings.shaders = [
      (getRetroShader "crt-hyllian.glsl")
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
      (getRetroShader "crt-lottes.glsl")
    ];
  };

  # Animation

  artcnn = mkFastProfileDef {
    name = "artcnn";
    desc = "ArtCNN (C4F16 DS)";
    shortcut = "CTRL+7";
    settings.shaders = [
      "${pkgs.artcnn}/share/artcnn/GLSL/ArtCNN_C4F16_DS.glsl"
    ];
  };

  artcnnHq = mkHqProfileDef {
    name = "artcnn-hq";
    desc = "ArtCNN (C4F32 DS)";
    shortcut = "CTRL+7";
    settings.shaders = [
      "${pkgs.artcnn}/share/artcnn/GLSL/ArtCNN_C4F32_DS.glsl"
    ];
  };

  # Anime 4k HQ

  anime4kAHq = mkHqProfileDef {
    name = "anime4k-a-hq";
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

  anime4kBHq = mkHqProfileDef {
    name = "anime4k-b-hq";
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

  anime4kCHq = mkHqProfileDef {
    name = "anime4k-c-hq";
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

  anime4kAAHq = mkHqProfileDef {
    name = "anime4k-a-a-hq";
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

  anime4kBBHq = mkHqProfileDef {
    name = "anime4k-b-b-hq";
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

  anime4kCAHq = mkHqProfileDef {
    name = "anime4k-c-a-hq";
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

  anime4kAFast = mkFastProfileDef {
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

  anime4kBFast = mkFastProfileDef {
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

  anime4kCFast = mkFastProfileDef {
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

  anime4kAAFast = mkFastProfileDef {
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

  anime4kBBFast = mkFastProfileDef {
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

  anime4kCAFast = mkFastProfileDef {
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
