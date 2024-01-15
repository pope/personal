{ ... }:

{
  imports = [
    ../../home
    ../../home/git.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;

    git.extraConfig = {
      core.sshCommand = "ssh.exe";
      "gpg \"ssh\"".program = "/mnt/c/Users/pope/AppData/Local/1Password/app/8/op-ssh-sign-wsl";
    };
  };

  my.home = {
    audio.enable = true;
    editor.enable = true;
    languages = {
      c.enable = true;
      go.enable = true;
      javascript.enable = true;
      python.enable = true;
      rust.enable = true;
    };
    lf.enable = true;
    packages.enable = true;
    shell.enable = true;
  };
}
