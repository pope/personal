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
      handbrake
      nvtopPackages.amd
      zathura
    ];

    stateVersion = "24.05";
  };

  wayland.windowManager.hyprland.settings.monitor = [
    "HDMI-A-2,2560x1440@120,0x0,auto"
    # "HDMI-A-1,preferred,0x0,auto"
    "DP-2,preferred,2560x0,auto"
  ];

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    anyrun.enable = false;
    browsers = {
      chromium.enable = true;
      firefox.enable = true;
    };
    dunst = {
      enable = true;
      font = "Sans 10";
    };
    editors = {
      emacs = {
        enable = true;
        useSymlink = true;
      };
      vscode.enable = true;
      neovim.enable = true;
    };
    email.enable = true;
    gaming.enable = true;
    git.enable = true;
    gnome.enable = true;
    gtk = {
      enable = true;
      disableQt = true;
      theme = "breeze";
    };
    hyprland = {
      enable = true;
      enableVrr = false;
    };
    hypridle = {
      enable = true;
      forDesktop = true;
      withPowerProfiles = true;
    };
    keymapp.enable = true;
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    mpv = {
      enable = true;
      enableHqAnimeSettings = true;
      defaultProfile = "fsr";
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
      crt.enable = true;
      ghostty = {
        enable = true;
        fontSize = 10;
      };
      foot.enable = true;
      kitty.enable = true;
      wezterm.enable = true;
    };
    theme.colorScheme = "tokyonight";
    tmux.enable = true;
    waybar = {
      enable = true;
      theme = "bubble";
    };
    xdg.enable = true;
    yazi.enable = true;
  };
}
