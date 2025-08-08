{ pkgs, config, lib, ... }:

# References
#
# - https://kokomins.wordpress.com/2019/10/14/mpv-config-guide/
# - https://iamscum.wordpress.com/guides/videoplayback-guide/mpv-conf/#auto-profiles
# - https://kohana.fi/article/mpv-for-anime

let
  defs = import ./defs.nix { inherit pkgs; };
  cfg = config.my.home.mpv;
in
{
  options.my.home.mpv = {
    enable = lib.mkEnableOption "mpv options";
    enableHqAnimeSettings = lib.mkEnableOption "use the HQ Anime4K shaders";
    enableFsr = lib.mkEnableOption "use FSR and make it the default profile";
    enableNvscaler = lib.mkEnableOption "use NVScaler and make it the default profile";
    enableVulkan = lib.mkOption {
      default = pkgs.stdenv.isLinux;
      example = true;
      description = "Whether to enable Vulkan GPU API.";
      type = lib.types.bool;
    };
    scale = lib.mkOption {
      type = lib.types.number;
      default = 1.5;
      example = 1;
      description = lib.mkDoc ''Scaling to apply to controls.'';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      mpvScripts.modernx # Included here so that the font is installed
      streamlink
      yt-dlp
    ];

    fonts.fontconfig.enable = true;

    programs.mpv = {
      enable = true;

      config = {
        # UI
        border = false;
        keep-open = true;
        osc = false;

        # Video
        vo = "gpu-next";
        hwdec = "auto-safe";
        fbo-format = "rgba16f";

        # Deband
        deband = true;
        deband-grain = 0;
        deband-iterations = 1;
        deband-range = 12;
        deband-threshold = 32;

        # Dithering
        dither-depth = "auto";
        dither = "fruit";

        # Track Selection
        slang = "en,eng";
        alang = "en,eng,ja,jp,jpn";

        profile = if cfg.enableFsr then defs.fsr.name else defs.generic.name;
      } // lib.optionalAttrs cfg.enableVulkan {
        gpu-api = "vulkan";
      };

      bindings =
        let
          mkBinding = def: { "${def.shortcut}" = ''apply-profile ${def.name}; show-text "Profile: ${def.desc}"''; };
        in
        lib.mkMerge [
          {
            "CTRL+0" = ''no-osd change-list glsl-shaders clr ""; show-text "GLSL shaders cleared"'';
          }

          (mkBinding defs.generic)
          (mkBinding defs.genericHigh)
          (lib.mkIf cfg.enableFsr (mkBinding defs.fsr))
          (lib.mkIf cfg.enableNvscaler (mkBinding defs.nvscaler))

          (mkBinding defs.crtGuestAdvancedNtsc)
          (mkBinding defs.crtLottes)

          (lib.mkIf cfg.enableHqAnimeSettings (mkBinding defs.anime4kAHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkBinding defs.anime4kBHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkBinding defs.anime4kCHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkBinding defs.anime4kAAHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkBinding defs.anime4kBBHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkBinding defs.anime4kCAHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkBinding defs.artcnn))

          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkBinding defs.anime4kAFast))
          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkBinding defs.anime4kBFast))
          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkBinding defs.anime4kCFast))
          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkBinding defs.anime4kAAFast))
          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkBinding defs.anime4kBBFast))
          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkBinding defs.anime4kCAFast))
        ];

      profiles =
        let
          mkProfile = def: { "${def.name}" = def.profile; };
        in
        lib.mkMerge [
          (mkProfile defs.generic)
          (mkProfile defs.genericHigh)
          (lib.mkIf cfg.enableFsr (mkProfile defs.fsr))
          (lib.mkIf cfg.enableNvscaler (mkProfile defs.nvscaler))

          (mkProfile defs.crtGuestAdvancedNtsc)
          (mkProfile defs.crtLottes)

          (lib.mkIf cfg.enableHqAnimeSettings (mkProfile defs.anime4kAHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkProfile defs.anime4kBHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkProfile defs.anime4kCHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkProfile defs.anime4kAAHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkProfile defs.anime4kBBHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkProfile defs.anime4kCAHq))
          (lib.mkIf cfg.enableHqAnimeSettings (mkProfile defs.artcnn))

          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkProfile defs.anime4kAFast))
          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkProfile defs.anime4kBFast))
          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkProfile defs.anime4kCFast))
          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkProfile defs.anime4kAAFast))
          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkProfile defs.anime4kBBFast))
          (lib.mkIf (!cfg.enableHqAnimeSettings) (mkProfile defs.anime4kCAFast))
        ];

      scripts = (with pkgs.mpvScripts; [
        modernx
        thumbfast
        visualizer
      ]) ++ lib.optionals pkgs.stdenv.isLinux [
        pkgs.mpvScripts.mpris
      ];

      scriptOpts = {
        osc = {
          scalefullscreen = cfg.scale;
          scalewindowed = cfg.scale;
          vidscale = false;
        };
      };
    };
  };
}
