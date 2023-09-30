{ pkgs, inputs, ... }:

{
  imports = [
    inputs.anyrun.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    ../../home/anyrun.nix
    ../../home/dunst.nix
    ../../home/gnome.nix
    ../../home/gtk.nix
    ../../home/hyprland
    ../../home/kitty
    ../../home/lf
    ../../home/packages.nix
    ../../home/xdg.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    packages = with pkgs; [
      firefox
    ];

    stateVersion = "23.05";
  };

  colorScheme = inputs.nix-colors.colorSchemes.rose-pine;

  wayland.windowManager.hyprland.settings.monitor = [
    "HDMI-A-1,2560x1440@120,0x0,auto"
    "DP-2,preferred,2560x0,auto"
  ];

  programs = {
    home-manager.enable = true;
  };
}
