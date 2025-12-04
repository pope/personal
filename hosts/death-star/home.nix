{ pkgs, ... }:

{
  imports = [
    ../../modules/home
  ];

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    package = pkgs.nix;
    settings = {
      # Add trusted-users to /etc/nix/nix.conf to make the warnings go away
      auto-optimise-store = true;
      builders-use-substitutes = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
    };
  };

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    packages = with pkgs; [
      nerd-fonts.gohufont
      nerd-fonts.lilex
      nerd-fonts.symbols-only
      nerd-fonts.terminess-ttf

      nixgl.nixGLIntel
      nixgl.nixVulkanIntel
    ];

    stateVersion = "24.11";
  };

  targets.genericLinux.enable = true;

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    editors = {
      emacs = {
        enable = true;
        package = pkgs.emacs;
      };
      neovim.enable = true;
    };
    git = {
      enable = true;
      sshCommand = "ssh.exe";
      opSshSignCommand = "/mnt/c/Users/pope/AppData/Local/Microsoft/WindowsApps/op-ssh-sign-wsl.exe";
    };
    languages.python.enable = true;
    packages.enable = true;
    shell.zsh.enable = true;
    ssh.enable = true;
    yazi.enable = true;
  };
}
