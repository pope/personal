{ config, lib, pkgs, ... }:

let
  inherit (config.my.nixos) mainUser;
  cfg = config.my.nixos.gaming;
in
{
  options.my.nixos.gaming = {
    enable = lib.mkEnableOption "gaming system options";
    enableSteam = lib.mkEnableOption "whether or not to enable Steam";
    ntsync = lib.mkEnableOption "whether or not to enable ntsync";
    preferredOutput = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      description = lib.mkDoc ''
        If specified, sets the preferred output for gamescope.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.ntsync -> lib.versionAtLeast config.boot.kernelPackages.kernel.version "6.14";
        message = "ntsync requires Linux 6.14+.";
      }
    ];

    boot.kernelModules = lib.mkIf cfg.ntsync [ "ntsync" ];

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

      gamemode.enable = true;

      steam = {
        enable = cfg.enableSteam;
        package = lib.mkIf cfg.ntsync (pkgs.steam.override {
          extraEnv = {
            MANGOHUD = false;
            PROTON_ENABLE_WAYLAND = 1;
            PROTON_USE_NTSYNC = 1;
          };
        });
        dedicatedServer.openFirewall = true;
        extraCompatPackages = lib.mkIf cfg.ntsync (with pkgs; [
          proton-ge-bin
        ]);
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
        protontricks.enable = true;
        remotePlay.openFirewall = true;
      };
    };

    # make ntsync device accessible
    services = {
      scx.enable = true;

      udev.packages =
        lib.mkIf cfg.ntsync
          [
            (pkgs.writeTextFile {
              name = "ntsync-udev-rules";
              text = ''KERNEL=="ntsync", MODE="0660", TAG+="uaccess"'';
              destination = "/etc/udev/rules.d/70-ntsync.rules";
            })
          ];
    };

    users.users."${mainUser}".extraGroups = [ "gamemode" ];
  };
}
