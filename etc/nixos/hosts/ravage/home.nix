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
      comic-code-ligatures
      dank-mono
      discord
      godot_4
      intel-gpu-tools
      lucida-grande
      monolisa
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
    browsers.firefox.enable = true;
    editors = {
      emacs.enable = true;
      neovim.enable = true;
      vscode.enable = true;
    };
    dunst.enable = true;
    dwl = {
      enable = true;
      terminalPackage = config.programs.ghostty.package;
    };
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
    multimedia.threed.enable = true;
    packages.enable = true;
    rofi.enable = true;
    shell.zsh.enable = true;
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
