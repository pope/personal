_:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    file.".face".source = ../../face.png;

    stateVersion = "25.05";
  };

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    browsers = {
      chromium.enable = true;
      firefox.enable = true;
    };
    editors = {
      emacs.enable = true;
      vscode.enable = true;
      neovim.enable = true;
    };
    git.enable = true;
    gnome.enable = false;
    gtk.enable = false;
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    packages.enable = true;
    shell.zsh.enable = true;
    ssh.enable = true;
    terminals = {
      ghostty.enable = true;
      kitty.enable = true;
    };
    theme.colorScheme = "tokyonight";
    tmux.enable = true;
    xdg.enable = true;
    yazi.enable = true;
  };
}
