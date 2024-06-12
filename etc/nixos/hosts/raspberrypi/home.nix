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
    dropbox.enable = true;
    dropbox.service.enable = true;
    packages.enable = true;
    shell.fish.enable = true;
    tmux.enable = true;
  };
}
