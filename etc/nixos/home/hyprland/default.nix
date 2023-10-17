{ pkgs, inputs, ... }:

let
  inherit (inputs.hyprland.packages.${pkgs.system}) hyprland;
  gamemode =
    pkgs.writeShellScriptBin "gamemode" ''
      HYPRGAMEMODE=$(${hyprland}/bin/hyprctl getoption animations:enabled | awk 'NR==2{print $2}')
      if [ "$HYPRGAMEMODE" = 1 ] ; then
          ${hyprland}/bin/hyprctl --batch "\
              keyword animations:enabled 0;\
              keyword decoration:drop_shadow 0;\
              keyword decoration:blur:enabled 0;\
              keyword general:gaps_in 0;\
              keyword general:gaps_out 0;\
              keyword general:border_size 1;\
              keyword decoration:rounding 0"
          exit
      fi
      ${hyprland}/bin/hyprctl reload
    '';
in
{
  imports = [
    ../waybar

    ./config.nix
  ];

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
      gamemode
      imv
      mpv
    ];

    # TODO(pope): Figure out how to do a "live" symlink here.
    # What's happening is that the hyprland.conf file, when saved, doesn't
    # update automatically.
    # file = {
    #   ".config/hypr" = {
    #     source = config.lib.file.mkOutOfStoreSymlink "/home/pope/Code/hyprland";
    #   };
    # };
  };

  xdg.configFile."swappy/config".text = ''
    [Default]
    save_dir=$HOME/Pictures/Screenshots
  '';

  systemd.user.targets.hyprland-session.Unit.Wants = [ "xdg-desktop-autostart.target" ];
}
