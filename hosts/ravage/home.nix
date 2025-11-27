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
      comic-code-ligatures
      discord
      godot_4
      intel-gpu-tools
      nvtopPackages.intel
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
    browsers = {
      chromium.enable = true;
      firefox.enable = true;
    };
    editors = {
      emacs = {
        enable = true;
        useSymlink = true;
      };
      neovim.enable = true;
      vscode.enable = true;
    };
    email.enable = true;
    dunst.enable = true;
    dwl.enable = false;
    git.enable = true;
    gnome = {
      enable = true;
      disableGnomeShellExtensions = true;
    };
    gtk = {
      enable = true;
      disableQt = true;
      theme = "breeze";
    };
    hypridle.enable = true;
    hyprland = {
      enable = true;
      enableBatterySaverMode = true;
    };
    kde.enable = true;
    keymapp.enable = true;
    languages.python.enable = true;
    mpv = {
      enable = true;
      scale = 1;
    };
    multimedia = {
      audio.enable = true;
      music.enable = true;
      threed.enable = true;
    };
    packages.enable = true;
    rofi = {
      enable = true;
      fontSize = 12;
    };
    shell.zsh.enable = true;
    sops.enable = true;
    ssh.enable = true;
    terminals = {
      ghostty.enable = true;
      wezterm.enable = true;
      foot.enable = true;
    };
    theme.colorScheme = "catppuccin";
    tmux.enable = true;
    waybar = {
      enable = true;
      theme = "bubble";
    };
    xdg.enable = true;
    yazi.enable = true;
  };
}
