{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.my.home.gnome;
in
{
  options.my.home.gnome = {
    enable = lib.mkEnableOption "GNOME home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs.gnomeExtensions; [
      app-icons-taskbar
      appindicator
      blur-my-shell
      caffeine
      forge
      pop-shell
      rounded-window-corners-reborn
      user-themes
    ];

    dconf.settings."org/gnome/shell" = {
      disable-user-extensions = false;

      # `gnome-extensions list` for a list
      enabled-extensions = with pkgs.gnomeExtensions; [
        appindicator.extensionUuid
        blur-my-shell.extensionUuid
        caffeine.extensionUuid
        rounded-window-corners-reborn.extensionUuid
        user-themes.extensionUuid
      ];
    };
  };
}
