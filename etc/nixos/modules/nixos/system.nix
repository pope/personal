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

      udev.packages = with pkgs; [
        keymapp
      ];
    };

    security.sudo.wheelNeedsPassword = false;
  };
}
