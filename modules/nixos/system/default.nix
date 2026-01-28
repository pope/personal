{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.nixos.system;
in
{
  options.my.nixos.system = {
    enable = lib.mkEnableOption "basic system options";
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.config = {
      # Allow unfree packages
      allowUnfree = true;
      firefox.speechSynthesisSupport = true;
      # Accept the joypixels license
      joypixels.acceptLicense = true;
    };

    boot.kernelPackages = pkgs.linuxPackages_latest;

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
      nixos-diff
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
    };
  };
}
