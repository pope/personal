{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # TODO(pope): Move this to maybe a browsers file.
    firefox

    gnomeExtensions.app-icons-taskbar
    gnomeExtensions.appindicator
    gnomeExtensions.blur-my-shell
    gnomeExtensions.caffeine
    gnomeExtensions.forge
    gnomeExtensions.pop-shell
    gnomeExtensions.rounded-window-corners
  ];
}
