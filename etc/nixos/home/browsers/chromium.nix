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
          # ublock origin
          id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
        }
        {
          # 1Password
          id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa";
        }
      ];
    };
  };
}
