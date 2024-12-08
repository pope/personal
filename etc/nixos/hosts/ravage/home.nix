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
      intel-gpu-tools
      nvtopPackages.intel
      stow
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
      neovim.enable = true;
      vscode.enable = false;
    };
    dunst.enable = false;
    git.enable = true;
    gnome.enable = false;
    gtk.enable = true;
    keymapp.enable = true;
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    lf.enable = false;
    mpv.enable = true;
    multimedia.threed.enable = true;
    packages.enable = true;
    shell.fish.enable = true;
    terminals = {
      kitty.enable = true;
      wezterm.enable = true;
      foot.enable = true;
    };
    theme.colorScheme = "catppuccin";
    tmux.enable = true;
    xdg.enable = true;
    yazi.enable = true;
  };
}
