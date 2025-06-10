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
    anyrun.enable = false;
    browsers = {
      chromium.enable = true;
      firefox.enable = true;
    };
    editors = {
      emacs.enable = true;
      neovim.enable = true;
      vscode.enable = true;
    };
    dunst.enable = true;
    dwl.enable = true;
    git.enable = true;
    gnome = {
      enable = true;
      disableGnomeShellExtensions = true;
    };
    gtk.enable = true;
    hypridle.enable = true;
    keymapp.enable = true;
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    lf.enable = false;
    mpv.enable = true;
    multimedia = {
      audio.enable = true;
      threed.enable = true;
    };
    music.enable = true;
    packages.enable = true;
    rofi = {
      enable = true;
      fontSize = 12;
    };
    shell.zsh.enable = true;
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
