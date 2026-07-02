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
      amdgpu_top
      discord
      godot_4
      nvtopPackages.amd
      hatsune-miku-cursor
    ];

    stateVersion = "25.05";
  };

  wayland.windowManager.hyprland.settings.monitor = [
    {
      output = "eDP-1";
      mode = "preferred";
      position = "auto";
      icc = ./BOE0CB4.icm;
      vrr = true;
      scale = 1.6666666666666;
    }
  ];

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    anyrun.enable = false;
    browsers = {
      librewolf.enable = true;
      chromium.enable = true;
    };
    cpu.arch = "znver4";
    editors = {
      emacs = {
        enable = true;
        useSymlink = true;
      };
      neovim.enable = true;
      vscode.enable = true;
      zed.enable = true;
    };
    dunst = {
      enable = true;
      font = "Sans 10";
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
      enable = true;
      # dpiScale = 2;
      enableBatterySaverMode = false;
      enableVrr = true;
    };
    kde.enable = true;
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    lf.enable = true;
    mpv = {
      enable = true;
      defaultProfile = "fsr";
    };
    multimedia = {
      audio.enable = true;
      graphics.enable = true;
      music = {
        enable = true;
        musicDirectory = "/media/cyberia/Public/Hi-Fi Music/";
      };
      photography.enable = true;
      threed = {
        enable = true;
        hip.enable = true;
      };
      video.enable = false;
    };
    obs.enable = true;
    packages.enable = true;
    rofi.enable = true;
    shell.zsh.enable = true;
    sops.enable = true;
    ssh.enable = true;
    terminals = {
      ghostty = {
        enable = true;
        fontSize = 10;
      };
      kitty = {
        enable = true;
        fontSize = 10;
      };
      foot = {
        enable = true;
        fontSize = 10;
      };
      wezterm.enable = true;
    };
    theme.colorScheme = "catppuccin";
    tmux.enable = true;
    waybar = {
      enable = true;
      scale = 0.9;
      theme = "bubble";
    };
    xdg.enable = true;
    yazi.enable = true;
  };
}
