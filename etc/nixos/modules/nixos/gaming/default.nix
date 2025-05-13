{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.gaming;
in
{
  options.my.nixos.gaming = {
    enable = mkEnableOption "gaming system options";
    enableSteam = mkEnableOption "whether or not to enable Steam";
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
