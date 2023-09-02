{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnomeExtensions.app-icons-taskbar
    gnomeExtensions.appindicator
    gnomeExtensions.blur-my-shell
    gnomeExtensions.caffeine
    gnomeExtensions.forge
    gnomeExtensions.pop-shell
    gnomeExtensions.rounded-window-corners
  ];
}
