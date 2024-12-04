_:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    stateVersion = "24.05";
  };

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    editors.neovim.enable = true;
    dropbox = {
      enable = false;
      service.enable = true;
    };
    packages.enable = true;
    shell.zsh.enable = true;
    tmux.enable = true;
    xdg.enable = true;
    yazi.enable = true;
  };
}

