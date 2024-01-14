{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.system;
in
{
  options.my.nixos.system = {
    enable = mkEnableOption "basic system options";
  };

  config = mkIf cfg.enable {

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

    services = {
      # Enable the OpenSSH daemon.
      openssh.enable = true;

      flatpak.enable = true;

      udev.extraRules = ''
        KERNEL=="hidraw*", ATTRS{idVendor}=="16c0", MODE="0664", GROUP="plugdev"
        KERNEL=="hidraw*", ATTRS{idVendor}=="3297", MODE="0664", GROUP="plugdev"
        # Rule for all ZSA keyboards
        SUBSYSTEM=="usb", ATTR{idVendor}=="3297", GROUP="plugdev"
        # Keymapp Flashing rules for the Voyager
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="3297", MODE:="0666", SYMLINK+="ignition_dfu"
      '';
    };

    security.sudo.wheelNeedsPassword = false;
  };
}
