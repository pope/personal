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
      lucida-grande
      godot_4
      nvtopPackages.amd
      rbutil
    ];

    stateVersion = "24.05";
  };

  wayland.windowManager.hyprland.settings = {
    monitor = [
      "eDP-1,preferred,auto,2,vrr,1"
    ];
    exec-once = [
      "systemctl --user start xsettingsd"
    ];
  };

  services.xsettingsd.settings =
    let
      scaling = 2;
      dpi = (96 * scaling) * 1024;
    in
    {
      "Xft/DPI" = dpi;
      "Gdk/UnscaledDPI" = dpi / scaling;
      "Gdk/WindowScalingFactor" = scaling;
    };
  xresources.properties."Xft.dpi" = 96 * 2;

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    anyrun.enable = true;
    browsers = {
      firefox.enable = true;
      chromium.enable = true;
    };
    editors = {
      neovim.enable = true;
      vscode.enable = true;
    };
    dunst.enable = true;
    git.enable = true;
    gnome.enable = true;
    gtk.enable = true;
    hyprland = {
      enable = true;
      hypridle.enable = true;
    };
    keymapp.enable = true;
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    lf.enable = true;
    mpv = {
      enable = true;
      enableHqAnimeSettings = true;
      # Disabled because MPV won't play videos otherwise. My current thought is
      # that when I added amdvlk, it stopped working.
      enableVulkan = false;
    };
    multimedia = {
      audio.enable = true;
      graphics.enable = true;
      photography.enable = true;
      threed = {
        enable = true;
        hip.enable = true;
      };
      video.enable = true;
    };
    obs.enable = true;
    packages.enable = true;
    shell.zsh.enable = true;
    terminals = {
      kitty.enable = true;
      foot.enable = true;
      wezterm.enable = true;
    };
    theme.colorScheme = "rose-pine";
    tmux.enable = true;
    xdg.enable = true;
    yazi.enable = true;
  };
}
