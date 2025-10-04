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
      # TODO(pope): Remove when https://github.com/NixOS/nixpkgs/issues/429268 is resolved.
      permittedInsecurePackages = [
        "libsoup-2.74.3"
      ];
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

      (pkgs.writeShellApplication {
        name = "nixos-diff";
        runtimeInputs = with pkgs; [
          coreutils
          findutils
          fzf
          gnugrep
          nvd
        ];
        text = # sh
          ''
            GEN_CUR=$(find /nix/var/nix/profiles \
                -name "system-*-link" \
                -printf "%CF %CH:%CM : %f -> %l\n" \
              | sort -r \
              | fzf --border --border-label "Select current generation" \
              | cut -d' ' -f6)
            GEN_PREV=$(find /nix/var/nix/profiles \
                -name "system-*-link" \
                -printf "%CF %CH:%CM : %f -> %l\n" \
              | sort -r \
              | grep -v "$GEN_CUR" \
              | fzf --border --border-label "Select previous generation" \
              | cut -d' ' -f6)

            nvd diff "$GEN_PREV" "$GEN_CUR"
          '';
      })
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
