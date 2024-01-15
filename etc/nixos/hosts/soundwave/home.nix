{ pkgs, inputs, ... }:

{
  imports = [
    inputs.anyrun.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    ../../home
    ../../home/anyrun.nix
    ../../home/chromium.nix
    ../../home/development.nix
    ../../home/dropbox.nix
    ../../home/dunst.nix
    ../../home/git.nix
    ../../home/gnome.nix
    ../../home/gtk.nix
    ../../home/hyprland
    ../../home/java.nix
    ../../home/keymapp.nix
    ../../home/kitty.nix
    ../../home/packages.nix
    ../../home/xdg.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    file.".face".source = ../../face.png;

    packages = with pkgs; [
      discord
      firefox
      nvtop
    ];

    stateVersion = "23.05";
  };

  colorScheme = inputs.nix-colors.colorSchemes.rose-pine;

  wayland.windowManager.hyprland.settings.monitor = [
    # "HDMI-A-1,2560x1440@119.998001,0x0,auto"
    "HDMI-A-1,preferred,0x0,auto"
    "DP-2,preferred,2560x0,auto"

    # "HDMI-A-1,disable"
    # "DP-2,preferred,0x0,auto"
  ];

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    languages = {
      c.enable = true;
    };
    lf.enable = true;
  };
}
