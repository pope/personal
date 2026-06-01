{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.home.hyprland;

  gamemode = pkgs.writeShellScriptBin "gamemode" ''
    HYPRGAMEMODE=$(${pkgs.hyprland}/bin/hyprctl getoption animations:enabled -j | ${pkgs.jq}/bin/jq '.bool')
    if [[ "$HYPRGAMEMODE" == "true" ]] ; then
        ${pkgs.hyprland}/bin/hyprctl eval 'hl.config({
          animations = { enabled = false },
          decoration = {
            blur = { enabled = false },
            shadow = { enabled = false}
          }
        })' &> /dev/null
        exit
    fi
    ${pkgs.hyprland}/bin/hyprctl reload
  '';
in
{
  options.my.home.hyprland = {
    enable = lib.mkEnableOption "hyprland home options";
    enableBatterySaverMode = lib.mkEnableOption "battery saving options";
    enableVrr = lib.mkEnableOption "variable refresh rate";
    dpiScale = lib.mkOption {
      type = lib.types.int;
      default = 1;
      description = lib.mkDoc ''
        The default DPI scale to use.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    wayland.windowManager.hyprland = {
      enable = true;
      configType = "lua";
      settings = import ./settings.nix { inherit config pkgs lib; };
      systemd.enable = false; # Using UWSM
      xwayland.enable = true;
    };

    xdg.configFile."uwsm/env-hyprland".text = # sh
      ''
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

    services.xsettingsd.settings = lib.mkIf (cfg.dpiScale != 1) (
      let
        dpi = (96 * cfg.dpiScale) * 1024;
      in
      {
        "Xft/DPI" = dpi;
        "Gdk/UnscaledDPI" = dpi / cfg.dpiScale;
        "Gdk/WindowScalingFactor" = cfg.dpiScale;
      }
    );
    xresources.properties = lib.mkIf (cfg.dpiScale != 1) {
      "Xft.dpi" = 96 * cfg.dpiScale;
    };

    systemd.user.targets.hyprland-session.Unit.Wants = [ "xdg-desktop-autostart.target" ];
  };
}
