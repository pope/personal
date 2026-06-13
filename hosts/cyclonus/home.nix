{ pkgs, ... }:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    packages = with pkgs; [
      _1password-cli
      _1password-gui
      nerd-fonts.hack
    ];

    stateVersion = "26.05";
  };

  my.home = {
    editors = {
      emacs.enable = true;
      neovim.enable = true;
    };
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

  targets.genericLinux.enable = true;
}
