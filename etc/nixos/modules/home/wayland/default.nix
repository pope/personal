{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.wayland;
in
{
  options.my.home.wayland = {
    enable = mkEnableOption "common Wayland options";
  };

  config = mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        alsa-utils
        grim
        imv
        libnotify
        pamixer
        slurp
        swappy
        swww
        wdisplays # Tool for managing displays
        wf-recorder
        wl-clipboard
        wlr-randr
      ];
    };

    # allow fontconfig to discover fonts and configurations installed through home.packages
    fonts.fontconfig.enable = true;

    programs.wlogout.enable = true;

    xdg.configFile = {
      "swappy/config".text = /* ini */ ''
        [Default]
        save_dir=$HOME/Pictures/Screenshots
      '';

      "uwsm/env".text = /* sh */ ''
        export WLR_NO_HARDWARE_CURSORS=1
        # Hint electron apps to use wayland
        export NIXOS_OZONE_WL=1
      '';
    };
  };
}
