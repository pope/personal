{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.browsers.chromium;
in
{
  options.my.home.browsers.chromium = {
    enable = mkEnableOption "Chromium browser home options";
  };

  config = mkIf cfg.enable {
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
      ];
    };
  };
}
