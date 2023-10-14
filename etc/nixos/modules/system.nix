{ pkgs, ... }:

{
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  # Accept the joypixels license
  nixpkgs.config.joypixels.acceptLicense = true;

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
      fira
      fira-go
      iosevka
      iosevka-comfy.comfy
      jetbrains-mono
      joypixels
      noto-fonts-emoji
      source-serif
      work-sans
      (nerdfonts.override { fonts = [ "FiraCode" "NerdFontsSymbolsOnly" ]; })
    ];

    enableDefaultPackages = true;

    fontconfig = {
      enable = true;

      antialias = true;
      defaultFonts = {
        emoji = [ "Joypixels" "Noto Color Emoji" ];
        monospace = [ "Iosevka" "FiraCode Nerd Font Mono" ];
        sansSerif = [ "Work Sans" "Fira Sans" "FiraGO" ];
        serif = [ "Source Serif" ];
      };
      hinting = {
        enable = true;
        autohint = false;
        style = "slight";
      };
      subpixel = {
        rgba = "rgb";
        lcdfilter = "light";
      };
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    flatpak
    git
    gparted
    ntfs3g
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
    openssh.enable = true;

    # Enable CUPS to print documents.
    printing.enable = true;

    flatpak.enable = true;
  };
}
