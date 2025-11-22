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

    stateVersion = "24.11";
  };

  wayland.windowManager.hyprland.settings.monitor = [
    ",preferred,auto,1,vrr,1"
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
    dunst.enable = false;
    editors = {
      vscode.enable = true;
      neovim.enable = true;
    };
    gaming.enable = true;
    git.enable = true;
    gnome.enable = true;
    gtk = {
      enable = true;
      disableQt = true;
      theme = "breeze";
    };
    hypridle = {
      enable = false;
      forDesktop = true;
      withPowerProfiles = true;
    };
    hyprland.enable = false;
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
      defaultProfile = "fsr";
    };
    multimedia = {
      audio.enable = true;
      graphics.enable = true;
      # Re-enable when CUDA support is fixed
      # https://github.com/NixOS/nixpkgs/issues/426836
      photography.enable = false;
      threed.enable = true;
      video.enable = false;
    };
    obs.enable = true;
    packages.enable = true;
    shell = {
      fish.enable = true;
      zsh.enable = true;
    };
    ssh.enable = true;
    terminals = {
      ghostty.enable = true;
      foot.enable = true;
      kitty.enable = true;
      wezterm.enable = true;
    };
    theme.colorScheme = "tokyonight";
    tmux.enable = true;
    waybar.enable = false;
    xdg.enable = true;
    yazi.enable = true;
  };
}
