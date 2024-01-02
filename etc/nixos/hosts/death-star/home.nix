{ pkgs, ... }:

{
  imports = [
    ../../home/audio.nix
    ../../home/development.nix
    ../../home/git.nix
    ../../home/packages.nix
    ../../home/lf.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    packages = with pkgs; [
      cmake
      gcc
      gnumake
      ninja
      stdenv
    ];

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
}
