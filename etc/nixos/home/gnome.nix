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
        appindicator.extensionUuid
        blur-my-shell.extensionUuid
        caffeine.extensionUuid
        custom-accent-colors.extensionUuid
        user-themes.extensionUuid
      ];
    };

    # "org/gnome/shell/extensions/user-theme".name = "Catppuccin-Mocha-Standard-Mauve-dark";
    "org/gnome/shell/extensions/custom-accent-colors" = {
      accent-color = "purple";
      theme-flatpak = true;
      theme-gtk3 = false;
      theme-shell = true;
    };
  };
}
