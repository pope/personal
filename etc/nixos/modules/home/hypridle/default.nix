{ config, pkgs, lib, ... }:

let
  cfg = config.my.home.hypridle;

  caffeinemode = pkgs.writeShellScriptBin "caffeinemode" ''
    if ${pkgs.procps}/bin/pidof hypridle > /dev/null
    then ${pkgs.systemd}/bin/systemctl --user stop hypridle.service && echo "hypridle stopped";
    else ${pkgs.systemd}/bin/systemctl --user start hypridle.service && echo "hypridle started";
    fi
  '';
in
{
  options.my.home.hypridle = {
    enable = lib.mkEnableOption "hypridle options";
    forDesktop = lib.mkEnableOption "desktop version of hypeidle configs";
    withPowerProfiles = lib.mkEnableOption "to enable power profile adjustments on idle";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ caffeinemode ];

    services.hypridle = {
      enable = true;
      settings = {
        general = {
          after_sleep_cmd = lib.optionalString
            config.my.home.hyprland.enable
            "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
          before_sleep_cmd =
            (lib.optionalString
              config.my.home.hyprland.enable
              "${pkgs.hyprland}/bin/hyprctl dispatch dpms off;")
            + "${pkgs.systemd}/bin/loginctl lock-session";
          ignore_dbus_inhibit = false;
          ignore_systemd_inhibit = false;
          lock_cmd = "${pkgs.procps}/bin/pidof hyprlock || ${pkgs.hyprlock}/bin/hyprlock";
        };
        listener = lib.optionals cfg.withPowerProfiles [
          {
            timeout = 180;
            on-timeout = "${pkgs.power-profiles-daemon}/bin/powerprofilesctl set power-saver";
            on-resume = "${pkgs.power-profiles-daemon}/bin/powerprofilesctl set balanced";
          }
        ] ++ lib.optionals (!cfg.forDesktop) [
          {
            # Dim the brightness of the screen.
            # The current value is stored so that when resumed, the brightness
            # will go back up to the original value - animated over time.
            timeout = 180;
            on-timeout = "${pkgs.brillo}/bin/brillo -O; ${pkgs.brillo}/bin/brillo -u 1000000 -S 10";
            on-resume = "${pkgs.brillo}/bin/brillo -I -u 500000";
          }
          {
            timeout = 300;
            on-timeout = "${pkgs.systemd}/bin/loginctl lock-session";
          }
          {
            timeout = 360;
            on-timeout = "${pkgs.systemd}/bin/systemctl suspend";
          }
        ];
      };
    };

    programs.hyprlock = {
      enable = true;
      settings = {
        background = [
          {
            path = "screenshot";
            blur_passes = 3;
            blur_size = 3;
            noise = 0.0117;
            contrast = 0.8916;
            brightness = 0.8172;
            vibrancy = 0.1696;
            vibrancy_darkness = 0.0;
          }
        ];
        input-field = [
          {
            placeholder_text = "";
          }
        ];
        label = [
          {
            text_align = "right";
            halign = "center";
            valign = "center";
            text = "Hi there, $USER";
            font_size = 50;
            font_family = "Sans";
            position = "0, 80";
          }
          {
            text_align = "right";
            halign = "center";
            valign = "center";
            text = "$TIME";
            font_size = 150;
            font_family = "Sans";
            position = "0, 300";
          }
        ];
      };
    };
  };
}
