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
    {
      output = "eDP-1";
      mode = "preferred";
      position = "auto";
      icc = ./LP140WF6_SPB7.icm;
      scale = 1;
    }
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
        package = pkgs.emacs.overrideAttrs (oldAttrs: {
          NIX_CFLAGS_COMPILE = "-march=skylake -mtune=skylake -O3";
          env = oldAttrs.env // {
            BYTE_COMPILE_EXTRA_FLAGS = ''
              --eval '(setq native-comp-speed 3)' \
              --eval '(setq native-comp-compiler-options '("-march=skylake" "-mtune=skylake" "-O3"))'
            '';
          };
        });
        useSymlink = true;
      };
      neovim.enable = true;
      vscode.enable = false;
      zed.enable = true;
    };
    email.enable = true;
    dunst = {
      enable = true;
      font = "Sans 11";
    };
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
    languages.python.enable = true;
    mpv = {
      enable = true;
      defaultProfile = "fast";
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
