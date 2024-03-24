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
      nvtopPackages.nvidia
    ];

    stateVersion = "23.05";
  };

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
    anyrun.enable = true;
    browsers = {
      chromium.enable = true;
      firefox.enable = true;
    };
    editors.vscode.enable = true;
    editors.neovim.enable = true;
    dropbox.enable = true;
    dunst.enable = true;
    git.enable = true;
    gnome.enable = true;
    gtk.enable = true;
    keymapp.enable = true;
    languages = {
      c.enable = true;
      go.enable = true;
      java.enable = true;
      javascript.enable = true;
      python.enable = true;
      rust.enable = true;
    };
    lf.enable = true;
    mpv = {
      enable = true;
      enableHqAnimeSettings = true;
    };
    packages.enable = true;
    shell.fish.enable = true;
    terminals = {
      crt.enable = true;
      kitty.enable = true;
      wezterm.enable = true;
    };
    tmux.enable = true;
    xdg.enable = true;
  };
}
