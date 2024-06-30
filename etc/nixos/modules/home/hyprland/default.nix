{ pkgs, inputs, ... }:

let
  inherit (inputs.hyprland.packages.${pkgs.system}) hyprland;
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
in
{
  imports = [
    ./config.nix
  ];

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
      gamemode
      grim
      hyprpicker
      imv
      libnotify
      pamixer
      slurp
      swappy
      swayidle
      swww
      wdisplays # Tool for managing displays
      wf-recorder
      wl-clipboard
      wlr-randr
    ];
  };

  programs = {
    wlogout.enable = true;
    swaylock.enable = true;
  };

  xdg.configFile."swappy/config".text = ''
    [Default]
    save_dir=$HOME/Pictures/Screenshots
  '';

  systemd.user.targets.hyprland-session.Unit.Wants = [ "xdg-desktop-autostart.target" ];
}
