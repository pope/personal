{ pkgs, ... }:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    packages = with pkgs; [
      nixgl.nixGLIntel
      nixgl.nixVulkanIntel
    ];

    stateVersion = "23.05";
  };

  targets.genericLinux.enable = true;

  programs = {
    home-manager.enable = true;
  };

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
    shell.fish.enable = true;
  };
}
