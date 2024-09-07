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
      (comic-code-ligatures.overrideAttrs (_: {
        src = /home/pope/Documents/fonts/comic-code-ligatures;
      }))
      discord
      (lucida-grande.overrideAttrs (_: {
        src = /home/pope/Documents/fonts/lucida-grande;
      }))
      godot_4
      nvtopPackages.amd
      rbutil
    ];

    stateVersion = "24.05";
  };

  wayland.windowManager.hyprland.settings.monitor = [
    "eDP-1,highres,auto,2,vrr,1"
  ];

  services.xsettingsd.settings =
    let
      # The actual PPI is 256, but 192 looks better to me.
      ppi = 192;
      scaling = 2;
      dpi = ppi * 1024;
    in
    {
      "Xft/DPI" = dpi;
      "Gdk/UnscaledDPI" = dpi / scaling;
      "Gdk/WindowScalingFactor" = scaling;
    };

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    anyrun.enable = true;
    browsers.firefox.enable = true;
    browsers.chromium.enable = true;
    editors = {
      neovim.enable = true;
      vscode.enable = true;
    };
    dunst.enable = true;
    git.enable = true;
    gnome.enable = true;
    gtk.enable = true;
    hyprland.enable = true;
    hyprland.hypridle.enable = true;
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
    multimedia = {
      audio.enable = true;
      graphics.enable = true;
      photography.enable = true;
      threed.enable = true;
      threed.hip.enable = true;
      video.enable = true;
    };
    obs.enable = true;
    packages.enable = true;
    shell.fish.enable = true;
    shell.zsh.enable = true;
    terminals = {
      kitty.enable = true;
      foot.enable = true;
      wezterm.enable = true;
    };
    theme.colorScheme = "rose-pine";
    tmux.enable = true;
    xdg.enable = true;
  };
}
