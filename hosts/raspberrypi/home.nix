_:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "pi";
    homeDirectory = "/home/pi";

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    editors.neovim.enable = true;
    dropbox = {
      enable = false;
      service.enable = false;
    };
    packages.enable = true;
    shell.fish.enable = true;
    ssh.enable = true;
    tmux.enable = true;
    yazi.enable = true;
  };
}
