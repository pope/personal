{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption;
  cfg = config.my.nixos.gaming;
in
{
  options.my.nixos.gaming = {
    enable = mkEnableOption "gaming system options";
    enableSteam = mkEnableOption "whether or not to enable Steam";
    preferredOutput = mkOption {
      type = with lib.types; nullOr str;
      default = null;
      description = lib.mkDoc ''
        If specified, sets the preferred output for gamescope.
      '';
    };
  };

  config = mkIf cfg.enable {
    # Hardware support from Steam
    hardware = {
      steam-hardware.enable = true;
      xone.enable = true;
      xpadneo.enable = true;
    };

    programs = {
      gamescope = {
        enable = true;
        capSysNice = true;
        args = [
          "--mangoapp"
        ] ++ lib.optionals (cfg.preferredOutput != null) [
          "--prefer-output"
          cfg.preferredOutput
        ];
      };

      steam = {
        enable = cfg.enableSteam;
        dedicatedServer.openFirewall = true;
        extraPackages = with pkgs; [
          gamemode
          gamescope
          mangohud
        ];
        gamescopeSession = {
          enable = true;
          env = {
            "MANGOHUD" = "true";
          };
        };
        localNetworkGameTransfers.openFirewall = true;
        remotePlay.openFirewall = true;
      };
    };
  };
}
