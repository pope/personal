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

      (pkgs.writeShellScriptBin "add-files-to-nix-store" ''
        nix-store --add-fixed sha256 \
            /media/cyberia/nix-files/fonts/*.tar.gz \
            /media/cyberia/nix-files/software/rns_344_linux_x86_64.tar.gz
      '')
      (pkgs.writeShellScriptBin "nixos-diff" /* sh */ ''
        # A script to select two nix generations and find the differences
        # between them.
        set -euo pipefail

        CUT=${lib.getExe' pkgs.coreutils "cut"}
        LS=${lib.getExe' pkgs.coreutils "ls"}
        FZF=${lib.getExe pkgs.fzf}
        GREP=${lib.getExe pkgs.gnugrep}
        NVD=${lib.getExe pkgs.nvd}

        GEN_CUR=$($LS /nix/var/nix/profiles/system-* -drlGg \
          | $FZF --border --border-label "Select current generation" \
          | $CUT -d' ' -f 7)
        GEN_PREV=$($LS /nix/var/nix/profiles/system-* -drlGg \
          | $GREP -v "$GEN_CUR" \
          | $FZF --border --border-label "Select previous generation" \
          | $CUT -d' ' -f 7)

        $NVD diff "$GEN_PREV" "$GEN_CUR"
      '')
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
