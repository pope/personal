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

  xdg = {
    cacheHome = config.home.homeDirectory + "/.cache";

    userDirs = {
      enable = true;
      createDirectories = true;
      extraConfig = {
        XDG_SCREENSHOTS_DIR = "${config.xdg.userDirs.pictures}/Screenshots";
      };
    };
  };

  programs = {
    home-manager.enable = true;
  };
}
