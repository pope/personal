{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.home.browsers.librewolf;
in
{
  options.my.home.browsers.librewolf = {
    enable = lib.mkEnableOption "Librewolf browser home options";
  };

  config = lib.mkIf cfg.enable {
    programs.librewolf = {
      enable = true;
      package = pkgs.unstable.librewolf;
      nativeMessagingHosts = lib.mkIf config.my.home.kde.enable [
        pkgs.kdePackages.plasma-browser-integration
      ];
      profiles.default = {
        isDefault = true;
      };
    };
  };
}
