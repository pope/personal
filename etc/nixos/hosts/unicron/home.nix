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
      discord
      godot_4
      nvtopPackages.nvidia
      zathura
    ];

    stateVersion = "24.05";
  };

  wayland.windowManager.hyprland.settings.monitor = [
    "HDMI-A-1,2560x1440@120,0x0,auto"
    # "HDMI-A-1,preferred,0x0,auto"
    "DP-2,preferred,2560x0,auto"
    # TODO(pope): Find out why there is this unknown screen showing up.
    # Leaving it means that if something gets opened on that monitor, then
    # Hyprland will crash.
    "Unknown-1,disable"
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
    editors = {
      vscode.enable = true;
      neovim.enable = true;
    };
    dunst = {
      enable = true;
      font = "Sans 10";
    };
    gaming.enable = true;
    git.enable = true;
    gnome.enable = true;
    gtk.enable = true;
    hyprland.enable = true;
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
    };
    multimedia = {
      audio.enable = true;
      graphics.enable = true;
      photography.enable = true;
      threed.enable = true;
      video.enable = false;
    };
    obs.enable = true;
    packages.enable = true;
    shell = {
      zsh.enable = true;
    };
    terminals = {
      crt.enable = true;
      ghostty = {
        enable = true;
        fontSize = 10;
      };
      foot.enable = true;
      kitty.enable = true;
      wezterm = {
        enable = true;
        useUnstable = true;
        useWayland = true;
      };
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
