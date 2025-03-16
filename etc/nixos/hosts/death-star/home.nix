{ pkgs, inputs, ... }:

{
  imports = [
    ../../modules/home
  ];

  nix = {
    gc = {
      automatic = true;
      frequency = "weekly";
      options = "--delete-older-than 7d";
    };
    registry = {
      nixpkgs-stable.flake = inputs.nixpkgs-stable;
    };
    package = pkgs.nix;
    settings = {
      # Add trusted-users to /etc/nix/nix.conf to make the warnings go away
      auto-optimise-store = true;
      builders-use-substitutes = true;
      experimental-features = [ "nix-command" "flakes" ];
    };
  };

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    packages = with pkgs; [
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
    editors.neovim.enable = true;
    git = {
      enable = true;
      sshCommand = "ssh.exe";
      opSshSignCommand = "/mnt/c/Users/pope/AppData/Local/1Password/app/8/op-ssh-sign-wsl";
    };
    languages.python.enable = true;
    packages.enable = true;
    shell.zsh.enable = true;
    yazi.enable = true;
  };
}
