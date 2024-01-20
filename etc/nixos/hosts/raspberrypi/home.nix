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
    packages.enable = true;
    shell.enable = true;
  };
}
