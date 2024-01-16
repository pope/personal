{ inputs, ... }:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;
  };

  colorScheme = inputs.nix-colors.colorSchemes.rose-pine;

  my.home = {
    audio.enable = true;
    editors.neovim.enable = true;
    git = {
      enable = true;
      sshCommand = "ssh.exe";
      opSshSignCommand = "/mnt/c/Users/pope/AppData/Local/1Password/app/8/op-ssh-sign-wsl";
    };
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
