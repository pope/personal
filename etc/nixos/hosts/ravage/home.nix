{ pkgs, inputs, ... }:

{
  imports = [
    inputs.anyrun.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    ../../home/anyrun.nix
    ../../home/development.nix
    ../../home/dunst.nix
    ../../home/git.nix
    ../../home/gnome.nix
    ../../home/gtk.nix
    ../../home/hyprland
    ../../home/keymapp.nix
    ../../home/kitty.nix
    ../../home/lf.nix
    ../../home/packages.nix
    ../../home/vscode.nix
    ../../home/xdg.nix
  ];

  nixpkgs.config.firefox.speechSynthesisSupport = true;

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    file.".face".source = ../../face.png;

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
