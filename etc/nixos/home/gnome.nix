{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnomeExtensions.app-icons-taskbar
    gnomeExtensions.appindicator
    gnomeExtensions.blur-my-shell
    gnomeExtensions.caffeine
    gnomeExtensions.custom-accent-colors
    gnomeExtensions.forge
    gnomeExtensions.pop-shell
    gnomeExtensions.rounded-window-corners
    gnomeExtensions.user-themes
  ];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };

    "org/gnome/shell" = {
      disable-user-extensions = false;

      # `gnome-extensions list` for a list
      enabled-extensions = with pkgs.gnomeExtensions; [
        custom-accent-colors.extensionUuid
        appindicator.extensionUuid
        user-themes.extensionUuid
        caffeine.extensionUuid
      ];
    };

    "org/gnome/shell/extensions/user-theme".name = "rose-pine-moon";
    "org/gnome/shell/extensions/custom-accent-colors" = {
      accent-color = "purple";
      theme-flatpak = true;
      theme-gtk3 = true;
      theme-shell = true;
    };
  };
}
