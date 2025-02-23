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
    enableVrr = mkEnableOption "variable refresh rate";
  };

  imports = [
    ./config.nix
  ];

  config = mkIf cfg.enable {
    wayland.windowManager.hyprland = {
      enable = true;
      systemd.enable = false; # Using UWSM
      xwayland.enable = true;
    };

    xdg.configFile."uwsm/env-hyprland".text = /* sh */ ''
      # For Firefox to run on Wayland
      export MOZ_ENABLE_WAYLAND=1
      export MOZ_WEBRENDER=1

      export XCURSOR_SIZE=24
      export HYPRCURSOR_SIZE=24

      export XDG_CURRENT_DESKTOP=Hyprland
      export XDG_SESSION_DESKTOP=Hyprland

      # For org.gtk.Settings.FileChooser
      export XDG_DATA_DIRS="$XDG_DATA_DIRS:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
    '';

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
