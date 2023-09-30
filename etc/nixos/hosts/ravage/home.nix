{ pkgs, inputs, ... }:

{
  imports = [
    inputs.anyrun.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    ../../home/anyrun.nix
    ../../home/dunst.nix
    ../../home/git.nix
    ../../home/gtk.nix
    ../../home/gnome.nix
    ../../home/hyprland
    ../../home/kitty.nix
    ../../home/lf.nix
    ../../home/packages.nix
    ../../home/xdg.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    packages = with pkgs; [
      discord
      # TODO(pope): Move this to maybe a browsers file.
      firefox
      intel-gpu-tools
      stow
    ];

    stateVersion = "23.05";
  };

  colorScheme = inputs.nix-colors.colorSchemes.rose-pine;

  wayland.windowManager.hyprland.settings.monitor = [
    "eDP-1,preferred,auto,1"
  ];

  programs = {
    home-manager.enable = true;
  };
}
