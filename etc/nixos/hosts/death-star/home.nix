{ ... }:

{
  imports = [
    ../../home
    ../../home/audio.nix
    ../../home/development.nix
    ../../home/git.nix
    ../../home/packages.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    git.extraConfig = {
      core.sshCommand = "ssh.exe";
      "gpg \"ssh\"".program = "/mnt/c/Users/pope/AppData/Local/1Password/app/8/op-ssh-sign-wsl";
    };
  };

  my.home = {
    languages = {
      c.enable = true;
    };
    lf.enable = true;
  };
}
