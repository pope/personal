{ config, pkgs, lib, inputs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.hyprland;

  inherit (inputs.hyprland.packages.${pkgs.system}) hyprland;
  inherit (inputs.hyprlock.packages.${pkgs.system}) hyprlock;
  gamemode =
    pkgs.writeShellScriptBin "gamemode" ''
      HYPRGAMEMODE=$(${hyprland}/bin/hyprctl getoption animations:enabled -j | ${pkgs.jq}/bin/jq '.int')
      if [ "$HYPRGAMEMODE" = 1 ] ; then
          ${hyprland}/bin/hyprctl --batch "\
              keyword animations:enabled 0;\
              keyword decoration:drop_shadow 0;\
              keyword decoration:blur:enabled 0;\
              keyword general:gaps_in 0;\
              keyword general:gaps_out 0;\
              keyword general:border_size 1;\
              keyword decoration:rounding 0" &> /dev/null
          exit
      fi
      ${hyprland}/bin/hyprctl reload
    '';
  caffeinemode = pkgs.writeShellScriptBin "caffeinemode" ''
    if ${pkgs.procps}/bin/pidof hypridle > /dev/null
    then ${pkgs.systemd}/bin/systemctl --user stop hypridle.service && echo "hypridle stopped";
    else ${pkgs.systemd}/bin/systemctl --user start hypridle.service && echo "hypridle started";
    fi
  '';
in
{
  options.my.home.hyprland = {
    enable = mkEnableOption "hyprland home options";
    hypridle.enable = mkEnableOption "Whether to enable hypridle";
  };

  imports = [
    ./config.nix
  ];

  config = mkIf cfg.enable {

    my.home.waybar.enable = true;

    wayland.windowManager.hyprland = {
      enable = true;
      package = hyprland;
      systemd.enable = true;
      xwayland.enable = true;
    };

    # allow fontconfig to discover fonts and configurations installed through home.packages
    fonts.fontconfig.enable = true;

    systemd.user.sessionVariables = {
      "NIXOS_OZONE_WL" = "1"; # for any ozone-based browser & electron apps to run on wayland
      "MOZ_ENABLE_WAYLAND" = "1"; # for firefox to run on wayland
      "MOZ_WEBRENDER" = "1";

      "XDG_SESSION_TYPE" = "wayland";
      "WLR_NO_HARDWARE_CURSORS" = "1";
      "WLR_EGL_NO_MODIFIRES" = "1";
    };

    home = {
      packages = with pkgs; [
        alsa-utils
        caffeinemode
        gamemode
        grim
        hyprpicker
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

    programs.wlogout.enable = true;

    programs.hyprlock = {
      enable = true;
      package = hyprlock;
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

    services.hypridle = {
      inherit (cfg.hypridle) enable;
      settings = {
        general = {
          after_sleep_cmd = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
          before_sleep_cmd = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off && ${pkgs.systemd}/bin/loginctl lock-session";
          ignore_dbus_inhibit = false;
          lock_cmd = "${pkgs.procps}/bin/pidof hyprlock || ${hyprlock}/bin/hyprlock";
        };
        listener = [
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

    services.hyprpaper = {
      enable = true;
      settings = {
        ipc = "on";
        splash = false;
        splash_offset = 2.0;

        preload = [ "~/Pictures/wallpaper-purple.png" ];
        wallpaper = [ ",~/Pictures/wallpaper-purple.png" ];
      };
    };

    xdg.configFile."swappy/config".text = ''
      [Default]
      save_dir=$HOME/Pictures/Screenshots
    '';

    systemd.user.targets.hyprland-session.Unit.Wants = [ "xdg-desktop-autostart.target" ];
  };
}
