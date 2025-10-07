{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.my.home) dwl hyprland;

  ExecCondition = ''
    ${pkgs.systemd}/lib/systemd/systemd-xdg-autostart-condition "wlroots:dwl-run:Hyprland" ""
  '';

  wayland-screenshot = pkgs.writeShellScriptBin "wayland-screenshot" ''
    ${lib.getExe pkgs.slurp} \
      | ${lib.getExe pkgs.grim} -g - - \
      | ${lib.getExe pkgs.swappy} -f -
  '';
in
{
  config = lib.mkIf (dwl.enable || hyprland.enable) {
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
        wayland-screenshot
        wdisplays # Tool for managing displays
        wf-recorder
        wl-clipboard
        wlr-randr
      ];
    };

    # allow fontconfig to discover fonts and configurations installed through home.packages
    fonts.fontconfig.enable = true;

    programs.wlogout = {
      enable = true;
      layout =
        let
          loginctl = lib.getExe' pkgs.systemd "loginctl";
          systemctl = lib.getExe' pkgs.systemd "systemctl";
          wayland-logout = lib.getExe pkgs.wayland-logout;
        in
        [
          {
            label = "lock";
            action = "${loginctl} lock-session";
            text = "Lock";
            keybind = "l";
          }
          {
            label = "hibernate";
            action = "${systemctl} hibernate";
            text = "Hibernate";
            keybind = "h";
          }
          {
            label = "logout";
            action = "${wayland-logout}";
            text = "Logout";
            keybind = "e";
          }
          {
            label = "shutdown";
            action = "${systemctl} poweroff";
            text = "Shutdown";
            keybind = "s";
          }
          {
            label = "suspend";
            action = "${systemctl} suspend";
            text = "Suspend";
            keybind = "u";
          }
          {
            label = "reboot";
            action = "${systemctl} reboot";
            text = "Reboot";
            keybind = "r";
          }
        ];
    };

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
        Install.WantedBy = [ "tile-manager-session.target" ];
        Service = {
          inherit ExecCondition;
          ExecStart = "${pkgs.udiskie}/bin/udiskie --appindicator";
        };
      };
    };

    xdg.configFile = {
      "swappy/config".text = # ini
        ''
          [Default]
          save_dir=$HOME/Pictures/Screenshots
        '';

      "uwsm/env".text = # sh
        ''
          export WLR_NO_HARDWARE_CURSORS=1
          # Hint electron apps to use wayland
          export NIXOS_OZONE_WL=1
        '';
    };
  };
}
