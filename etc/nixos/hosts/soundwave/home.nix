{ pkgs, ... }:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    file.".face".source = ../../face.png;

    packages = with pkgs; [
      blender
      bitwig-studio
      discord
      godot_4
      nvtopPackages.nvidia
    ];

    stateVersion = "23.05";
  };

  wayland.windowManager.hyprland.settings.monitor = [
    # "HDMI-A-1,2560x1440@119.998001,0x0,auto"
    "HDMI-A-1,preferred,0x0,auto"
    "DP-2,preferred,2560x0,auto"
    # TODO(pope): Find out why there is this unknown screen showing up.
    # Leaving it means that if something gets opened on that monitor, then
    # Hyprland will crash.
    "Unknown-1,disable"

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
    dropbox = {
      enable = true;
      gui.enable = true;
    };
    dunst.enable = true;
    gaming.enable = true;
    git.enable = true;
    gnome.enable = true;
    gtk.enable = true;
    hyprland.enable = true;
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
    obs.enable = true;
    packages.enable = true;
    shell.fish.enable = true;
    shell.zsh.enable = true;
    terminals = {
      crt.enable = true;
      kitty.enable = true;
      wezterm.enable = true;
    };
    tmux.enable = true;
    xdg.enable = true;
  };
}
