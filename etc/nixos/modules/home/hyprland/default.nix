{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.hyprland;

  gamemode =
    pkgs.writeShellScriptBin "gamemode" ''
      HYPRGAMEMODE=$(${pkgs.hyprland}/bin/hyprctl getoption animations:enabled -j | ${pkgs.jq}/bin/jq '.int')
      if [ "$HYPRGAMEMODE" = 1 ] ; then
          ${pkgs.hyprland}/bin/hyprctl --batch "\
              keyword animations:enabled 0;\
              keyword decoration:rounding 0;\
              keyword decoration:drop_shadow 0;\
              keyword decoration:blur:enabled 0;\
              keyword decoration:shadow:enabled 0;\
              keyword general:gaps_in 0;\
              keyword general:gaps_out 0;\
              keyword general:border_size 1" &> /dev/null
          exit
      fi
      ${pkgs.hyprland}/bin/hyprctl reload
    '';
in
{
  options.my.home.hyprland = {
    enable = mkEnableOption "hyprland home options";
  };

  imports = [
    ./config.nix
  ];

  config = mkIf cfg.enable {

    my.home = {
      waybar.enable = true;
      wayland.enable = true;
    };

    wayland.windowManager.hyprland = {
      enable = true;
      systemd.enable = true;
      xwayland.enable = true;
    };

    systemd.user.sessionVariables = {
      "NIXOS_OZONE_WL" = "1"; # for any ozone-based browser & electron apps to run on wayland
      "MOZ_ENABLE_WAYLAND" = "1"; # for firefox to run on wayland
      "MOZ_WEBRENDER" = "1";

      "XDG_SESSION_TYPE" = "wayland";
      "WLR_NO_HARDWARE_CURSORS" = "1";
      "WLR_EGL_NO_MODIFIRES" = "1";
    };

    home.packages = with pkgs; [
      gamemode
      hyprpicker
    ];

    services.hyprpaper = {
      # Disabling in favor of swww.
      # With swww, I can change the background easily after the fact.
      enable = false;
      settings = {
        ipc = "on";
        splash = false;
        splash_offset = 2.0;

        preload = [ "~/Pictures/wallpaper-purple.png" ];
        wallpaper = [ ",~/Pictures/wallpaper-purple.png" ];
      };
    };

    systemd.user.targets.hyprland-session.Unit.Wants = [ "xdg-desktop-autostart.target" ];
  };
}
