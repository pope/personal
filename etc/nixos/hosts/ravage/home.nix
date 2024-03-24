{ pkgs, ... }:

{
  imports = [
    ../../modules/home
    ../../modules/home/hyprland
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    file.".face".source = ../../face.png;

    packages = with pkgs; [
      discord
      intel-gpu-tools
      stow
    ];

    stateVersion = "23.05";
  };

  wayland.windowManager.hyprland.settings.monitor = [
    "eDP-1,preferred,auto,1"
  ];

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    anyrun.enable = true;
    browsers.firefox.enable = true;
    editors = {
      neovim.enable = true;
      vscode.enable = false;
    };
    dunst.enable = true;
    git.enable = true;
    gnome.enable = true;
    gtk.enable = true;
    keymapp.enable = true;
    languages = {
      c.enable = true;
      go.enable = true;
      javascript.enable = true;
      python.enable = true;
      rust.enable = true;
    };
    lf.enable = true;
    mpv.enable = true;
    packages.enable = true;
    shell.fish.enable = true;
    terminals.kitty.enable = true;
    terminals.wezterm.enable = true;
    theme.colorScheme = "catppuccin";
    tmux.enable = true;
    xdg.enable = true;
  };
}
