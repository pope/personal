{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.wayland;

  ExecCondition = ''
    ${pkgs.systemd}/lib/systemd/systemd-xdg-autostart-condition "wlroots:dwl-run:Hyprland" ""
  '';
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

    systemd.user.services = {
      polkit-gnome-authentication-agent-1 = {
        Unit = {
          Description = "polkit-gnome-authentication-agent-1";
          After = [ "graphical-session.target" ];
          Wants = [ "graphical-session.target" ];
          ConditionEnvironment = [ "XDG_SESSION_TYPE=wayland" ];
        };
        Install.WantedBy = [ "graphical-session.target" ];
        Service = {
          inherit ExecCondition;
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };

      swww = {
        Unit = {
          Description = "Efficient animated wallpaper daemon for wayland";
          PartOf = [ "graphical-session-pre.target" ];
          After = [ "graphical-session.target" ];
          ConditionEnvironment = [ "XDG_SESSION_TYPE=wayland" ];
        };
        Install.WantedBy = [ "graphical-session.target" ];
        Service = {
          inherit ExecCondition;
          ExecStart = "${pkgs.swww}/bin/swww-daemon";
          ExecStop = "${pkgs.swww}/bin/swww kill";
          Restart = "always";
          RestartSec = 10;
        };
      };

      # Using this instead of `service.udiskie` since the latter requires
      # the tray.target - and I don't feel like setting that up.
      udiskie = {
        Unit = {
          After = [ "graphical-session-pre.target" ];
          ConditionEnvironment = [ "XDG_SESSION_TYPE=wayland" ];
          PartOf = [ "graphical-session.target" ];
        };
        Install.WantedBy = [ "graphical-session.target" ];
        Service = {
          inherit ExecCondition;
          # TODO(pope): See if `--appindicator` is needed when Hyprland goes
          # through UWSM.
          ExecStart = "${pkgs.udiskie}/bin/udiskie --appindicator";
        };
      };
    };

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
