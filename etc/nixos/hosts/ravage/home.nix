{ config, pkgs, inputs, ... }:

{
  imports = [
    inputs.anyrun.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    ../../home/anyrun.nix
    ../../home/dunst.nix
    ../../home/git.nix
    ../../home/gtk.nix
    # ../../home/gnome.nix
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
      # TODO(pope): Move this to maybe a browsers file.
      firefox
      intel-gpu-tools
      stow
    ];

    stateVersion = "23.05";
  };

  colorScheme = inputs.nix-colors.colorSchemes.rose-pine;

  programs = {
    home-manager.enable = true;
  };
}
