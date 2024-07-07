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
      bitwig-studio
      discord
      nvtopPackages.amd
    ];

    stateVersion = "24.05";
  };

  wayland.windowManager.hyprland.settings.monitor = [
    "eDP-1,highres,auto,1.333333"
  ];

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    anyrun.enable = true;
    browsers.firefox.enable = true;
    browsers.chromium.enable = true;
    editors = {
      neovim.enable = true;
      vscode.enable = false;
    };
    dunst.enable = true;
    git.enable = true;
    gnome.enable = true;
    gtk.enable = true;
    hyprland.enable = true;
    keymapp.enable = true;
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    lf.enable = true;
    mpv = {
      enable = true;
      enableHqAnimeSettings = true;
    };
    packages.enable = true;
    shell.fish.enable = true;
    shell.zsh.enable = true;
    terminals.kitty.enable = true;
    terminals.wezterm.enable = true;
    theme.colorScheme = "rose-pine";
    tmux.enable = true;
    xdg.enable = true;
  };
}
