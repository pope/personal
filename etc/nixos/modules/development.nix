{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    android-studio
    cmake
    gradle
    fzf
    gcc
    gh
    gnumake
    go
    ninja
    nodejs
    python3Full
    ripgrep
    stdenv
    tldr
    tree
  ];

  programs = {
    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # mtr.enable = true;
    # gnupg.agent = {
    #   enable = true;
    #   enableSSHSupport = true;
    # };

    java.enable = true;

    neovim = {
      enable = true;
      defaultEditor = true;
    };
  };
}

