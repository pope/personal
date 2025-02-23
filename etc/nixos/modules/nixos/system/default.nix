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
    nixpkgs.config = {
      # Allow unfree packages
      allowUnfree = true;
      firefox.speechSynthesisSupport = true;
      # Accept the joypixels license
      joypixels.acceptLicense = true;
    };

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

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    environment.systemPackages = with pkgs; [
      git
      gparted
      ntfs3g
      wget
    ];

    security.sudo.wheelNeedsPassword = false;

    services = {
      # Enable the OpenSSH daemon.
      openssh = {
        enable = true;
        settings.PasswordAuthentication = false;
      };

      udev.extraRules = ''
        ## Needed Zsa and Voyager
        KERNEL=="hidraw*", ATTRS{idVendor}=="16c0", MODE="0664", GROUP="plugdev"
        KERNEL=="hidraw*", ATTRS{idVendor}=="3297", MODE="0664", GROUP="plugdev"
        # Rule for all ZSA keyboards
        SUBSYSTEM=="usb", ATTR{idVendor}=="3297", GROUP="plugdev"
        # Keymapp Flashing rules for the Voyager
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="3297", MODE:="0666", SYMLINK+="ignition_dfu"
      '';
    };
  };
}
