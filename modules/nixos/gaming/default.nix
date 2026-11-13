{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.my.nixos) mainUser;
  cfg = config.my.nixos.gaming;
  musnixEnabled = config.musnix.enable or false;
  swappiness = 10;
in
{
  options.my.nixos.gaming = {
    enable = lib.mkEnableOption "gaming system options";
    steam.enable = lib.mkEnableOption "Steam";
    ntsync.enable = lib.mkEnableOption "ntsync";
    scx.enable = lib.mkEnableOption "scx scheduler";
    preferredOutput = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      description = ''
        If specified, sets the preferred output for gamescope.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion =
          cfg.ntsync.enable -> lib.versionAtLeast config.boot.kernelPackages.kernel.version "6.14";
        message = "ntsync requires Linux 6.14+.";
      }
      {
        assertion = musnixEnabled -> (config.boot.kernel.sysctl."vm.swappiness" or null) == swappiness;
        message = "Expected musnix to set vm.swappiness to ${toString swappiness}.";
      }
    ];

    boot = {
      kernelParams = [
        # Disable periodic NMI watchdog interrupts to reduce latency and save laptop power.
        "nowatchdog"
        # Disable split-lock/bus-lock #DB traps and log spam from Steam/Proton.
        "split_lock_detect=off"
      ];
      kernel.sysctl = {
        # Disable proactive background memory compaction to prevent 1% low frame stutters.
        "vm.compaction_proactiveness" = 0;
        # Flush dirty pages to NVMe in small fixed chunks (64 MiB bg / 256 MiB max) instead of multi-GB bursts.
        "vm.dirty_background_bytes" = 67108864;
        "vm.dirty_bytes" = 268435456;
        # Reduce page lock hold fairness to lower tail latency when streaming game assets.
        "vm.page_lock_unfairness" = 1;
        # Prefer keeping game/app memory in RAM over page cache (deferred when musnix already sets it).
        "vm.swappiness" = lib.mkIf (!musnixEnabled) swappiness;
        # Prevent kswapd from aggressively reclaiming memory after fragmentation events.
        "vm.watermark_boost_factor" = 0;
      };
      kernelModules = lib.mkIf cfg.ntsync.enable [ "ntsync" ];
    };

    systemd.tmpfiles.rules = [
      # Defer THP defrag to khugepaged unless explicitly requested via madvise.
      "w /sys/kernel/mm/transparent_hugepage/defrag - - - - defer+madvise"
      # Allow Proton/Mesa shader caches on tmpfs/shmem to opt into 2 MiB hugepages.
      "w /sys/kernel/mm/transparent_hugepage/shmem_enabled - - - - advise"
    ];

    # Hardware support from Steam
    hardware = {
      steam-hardware.enable = true;
      xone.enable = true;
      xpadneo.enable = true;
    };

    programs = {
      gamescope = {
        enable = true;
        # Disabling per https://github.com/NixOS/nixpkgs/issues/523200
        capSysNice = false;
        args = [
          "--mangoapp"
        ]
        ++ lib.optionals (cfg.preferredOutput != null) [
          "--prefer-output"
          cfg.preferredOutput
        ];
      };

      gamemode.enable = true;

      steam = {
        enable = cfg.steam.enable;
        package = lib.mkIf cfg.ntsync.enable (
          pkgs.steam.override {
            extraEnv = {
              MANGOHUD = false;
              PROTON_ENABLE_WAYLAND = 1;
              PROTON_USE_NTSYNC = 1;
            };
          }
        );
        dedicatedServer.openFirewall = true;
        extraCompatPackages = lib.mkIf cfg.ntsync.enable (
          with pkgs;
          [
            proton-ge-bin
          ]
        );
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
      scx-loader = {
        enable = cfg.scx.enable;
        config.default_sched = "scx_lavd";
      };

      udev.packages = lib.mkIf cfg.ntsync.enable [
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
