{ config, pkgs, ... }:

{
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-old-than 1w";
    };
    settings = {
      auto-optimise-store = true;
      builders-use-substitutes = true;
      # Enable Flakes and the new command-line tool
      experimental-features = [ "nix-command" "flakes" ];
    };
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";

    extraLocaleSettings = {
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };
  };

  fonts.packages = with pkgs; [
    # fira-code
    # fira-code-symbols
    iosevka
    iosevka-comfy.comfy
    jetbrains-mono
    (nerdfonts.override { fonts = [ "FiraCode" "NerdFontsSymbolsOnly" ]; })
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    _1password 
    _1password-gui 
    android-studio
    cmake
    discord
    gradle
    flameshot
    flatpak
    fzf
    gcc
    gh
    git
    gnumake
    go
    goverlay
    gparted
    htop
    kitty
    kitty-themes
    lf
    lutris
    mangohud
    neofetch
    ninja
    nodejs
    nvtop
    obs-studio
    python3Full
    qemu
    ripgrep
    stdenv
    tldr
    tree
    virt-manager
    virtiofsd
    vkbasalt
    wget
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

    steam = {
      enable = true;
      # remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      # dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    };

    neovim = {
      enable = true;
      defaultEditor = true;
    };
  };

  services = {
    # Enable the OpenSSH daemon.
    # openssh.enable = true;

    # Enable CUPS to print documents.
    printing.enable = true;

    flatpak.enable = true;
  };
}
