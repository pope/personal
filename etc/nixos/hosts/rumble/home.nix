{ config, pkgs, ... }:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    file.".face".source = ../../face.png;

    packages = with pkgs; [
      amdgpu_top
      discord
      godot_4
      nvtopPackages.amd
      rbutil
      hatsune-miku-cursor
    ];

    stateVersion = "25.05";
  };

  wayland.windowManager.hyprland.settings.monitor = [
    "eDP-1,preferred,auto,2,vrr,1"
  ];

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    anyrun.enable = false;
    browsers = {
      firefox.enable = true;
      chromium.enable = true;
    };
    editors = {
      emacs = {
        enable = true;
        useSymlink = true;
      };
      neovim.enable = true;
      vscode.enable = true;
    };
    dunst.enable = true;
    dwl = {
      enable = true;
      dpiScale = 2;
      terminalPackage = config.programs.ghostty.package;
    };
    email.enable = true;
    git.enable = true;
    gnome.enable = true;
    gtk = {
      enable = true;
      disableQt = true;
      theme = "breeze";
    };
    hypridle.enable = true;
    hyprland = {
      enable = false;
      dpiScale = 2;
      enableBatterySaverMode = true;
      enableVrr = true;
    };
    keymapp.enable = true;
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    lf.enable = true;
    mpv = {
      enable = true;
      enableFsr = true;
    };
    multimedia = {
      audio.enable = true;
      graphics.enable = true;
      photography.enable = true;
      threed = {
        enable = true;
        hip.enable = true;
      };
      video.enable = false;
    };
    music.enable = true;
    obs.enable = true;
    packages.enable = true;
    rofi.enable = true;
    shell.zsh.enable = true;
    sops.enable = true;
    ssh.enable = true;
    terminals = {
      ghostty = {
        enable = true;
        fontSize = 9;
      };
      kitty.enable = false;
      foot = {
        enable = true;
        fontSize = 9;
      };
      wezterm = {
        enable = false;
        useWayland = true;
      };
    };
    theme.colorScheme = "catppuccin";
    tmux.enable = true;
    waybar = {
      enable = true;
      scale = 0.7;
      theme = "bubble";
    };
    xdg.enable = true;
    yazi.enable = true;
  };
}
