{ pkgs, ... }:

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
      trusted-users = [ "pope" ];
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

  fonts = {
    fontDir.enable = true;
    packages = with pkgs; [
      # fira-code
      # fira-code-symbols
      iosevka
      iosevka-comfy.comfy
      jetbrains-mono
      (nerdfonts.override { fonts = [ "FiraCode" "NerdFontsSymbolsOnly" ]; })
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    discord
    flameshot
    flatpak
    git
    gparted
    htop
    kitty
    kitty-themes
    neofetch
    nvtop
    qemu
    virt-manager
    virtiofsd
    wget
  ];

  programs = {
    _1password.enable = true;
    _1password-gui = {
      enable = true;
      polkitPolicyOwners = [ "pope" ];
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
