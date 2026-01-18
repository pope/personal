{ pkgs, inputs, ... }:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "deck";
    homeDirectory = "/home/deck";

    packages = with pkgs; [
      _1password-cli
      _1password-gui
      nerd-fonts.hack
    ];

    stateVersion = "25.11";
  };

  my.home = {
    editors.neovim.enable = true;
    git.enable = true;
    packages.enable = true;
    shell.zsh.enable = true;
    ssh.enable = true;
    terminals.ghostty.enable = true;
    tmux.enable = true;
    yazi.enable = true;
  };

  programs = {
    home-manager.enable = true;
  };

  targets.genericLinux = {
    enable = true;
    nixGL = {
      installScripts = [ "mesa" ];
      inherit (inputs.nixgl) packages;
      vulkan.enable = true;
    };
  };
}
