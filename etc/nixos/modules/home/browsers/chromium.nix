{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.browsers.chromium;
in
{
  options.my.home.browsers.chromium = {
    enable = lib.mkEnableOption "Chromium browser home options";
  };

  config = lib.mkIf cfg.enable {
    programs.chromium = {
      enable = true;
      dictionaries = [
        pkgs.hunspellDictsChromium.en_US
      ];
      extensions = [
        {
          # uBlock Origin Lite
          id = "ddkjiahejlhfcafbddmgiahcphecmpfh";
        }
        {
          # Privacy Badger
          id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
        }
        {
          # 1Password
          id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa";
        }
      ]
      ++ lib.optionals config.my.home.kde.enable [
        {
          # Plasma Integration
          id = "cimiefiiaegbelhefglklhhakcgmhkai";
        }
      ];
      nativeMessagingHosts = lib.mkIf config.my.home.kde.enable [
        pkgs.kdePackages.plasma-browser-integration
      ];
    };
  };
}
