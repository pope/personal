{ config, pkgs, ... }:

let
  bg = "23.png";
in
{
  wayland.windowManager.hyprland.settings = {
    monitor = [
      "eDP-1,preferred,auto,1"
    ];
    exec-once = [
      "${pkgs.dunst}/bin/dunst"
      "${pkgs.waybar}/bin/waybar"
      "${pkgs.swww}/bin/swww init --no-daemon"
      "${pkgs.networkmanagerapplet}/bin/nm-applet --indicator"
      "${pkgs.udiskie}/bin/udiskie --appindicator --no-password-prompt"
      "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
    ];
    exec = [
      "sleep 3 && ${pkgs.swww}/bin/swww img ${config.xdg.userDirs.pictures}/${bg}"
    ];

    xwayland.force_zero_scaling = true;

    input = {
      kb_layout = "us";
      kb_variant = "";
      kb_model = "";
      kb_options = "";
      kb_rules = "";

      follow_mouse = 1;

      accel_profile = "flat";
      force_no_accel = 1;
      numlock_by_default = 1;
      repeat_delay = 160;
      repeat_rate = 25;
      sensitivity = 0;

      touchpad = {
        natural_scroll = 1;
      };
    };

    general = {
      gaps_in = 4;
      gaps_out = 8;
      border_size = 3;
      apply_sens_to_raw = 1; # whether to apply the sensitivity to raw input (e.g. used by games where you aim using your mouse)
      "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
      "col.inactive_border" = "rgba(595959aa)";
      layout = "dwindle";
    };

    decoration = {
      "col.shadow" = "rgba(1a1a1aee)";
      drop_shadow = true;
      multisample_edges = true;
      rounding = 10;
      shadow_ignore_window = true;
      shadow_range = 20;
      shadow_render_power = 3;
      blur = {
        enabled = true;
        brightness = 1;
        contrast = 1.5;
        ignore_opacity = true;
        new_optimizations = true;
        noise = 0.0117;
        passes = 3;
        size = 6;
      };
    };

    animations = {
      enabled = false;
      # Selmer443 config
      bezier = [
        "pace,0.46, 1, 0.29, 0.99"
        "overshot,0.13,0.99,0.29,1.1"
        "md3_decel, 0.05, 0.7, 0.1, 1"
      ];
      animation = [
        "windowsIn,1,6,md3_decel,slide"
        "windowsOut,1,6,md3_decel,slide"
        "windowsMove,1,6,md3_decel,slide"
        "fade,1,10,md3_decel"
        "workspaces,1,9,md3_decel,slide"
        "workspaces, 1, 6, default"
        "specialWorkspace,1,8,md3_decel,slide"
        "border,1,10,md3_decel"
      ];
    };

    misc = {
      disable_hyprland_logo = true;
      disable_splash_rendering = true;

      vfr = true; # misc:no_vfr -> misc:vfr. bool, heavily recommended to leave at default on. Saves on CPU usage.
      vrr = false; # misc:vrr -> Adaptive sync of your monitor. 0 (off), 1 (on), 2 (fullscreen only). Default 0 to avoid white flashes on select hardware.
    };

    dwindle = {
      default_split_ratio = 1.0;
      force_split = 0;
      no_gaps_when_only = false;
      preserve_split = true;
      pseudotile = true; # enable pseudotiling on dwindle
      special_scale_factor = 0.8;
      split_width_multiplier = 1.0;
      use_active_for_splits = true;
    };

    master = {
      # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
      new_is_master = true;
    };

    gestures = {
      workspace_swipe = false;
    };

    debug = {
      damage_tracking = 2; # leave it on 2 (full) unless you hate your GPU and want to make it suffer!
    };

    bind = [
      "SUPER, Q, exec, kitty"
      "SUPER, RETURN, exec, kitty"
      "SUPER, C, killactive"
      "SUPER, M, exit"
      "SUPER, E, exec, thunar"
      "SUPER, V, togglefloating"
      "SUPER, R, exec, wofi --show drun"
      "SUPER, P, pseudo" # dwindle
      "SUPER, J, togglesplit" # dwindle
      "SUPER, S, exec, rofi -show drun -show-icons"

      "SUPER, left, movefocus, l"
      "SUPER, right, movefocus, r"
      "SUPER, up, movefocus, u"
      "SUPER, down, movefocus, d"

      "SUPER SHIFT, left, movewindow, l"
      "SUPER SHIFT, right, movewindow, r"
      "SUPER SHIFT, up, movewindow, u"
      "SUPER SHIFT, down, movewindow, d"

      "SUPER ALT, right, resizeactive, 50 0"
      "SUPER ALT, left, resizeactive, -50 0"
      "SUPER ALT, up, resizeactive, 0 -50"
      "SUPER ALT, down, resizeactive, 0 50"

      "SUPER, 1, workspace, 1"
      "SUPER, 2, workspace, 2"
      "SUPER, 3, workspace, 3"
      "SUPER, 4, workspace, 4"
      "SUPER, 5, workspace, 5"
      "SUPER, 6, workspace, 6"
      "SUPER, 7, workspace, 7"
      "SUPER, 8, workspace, 8"

      "SUPER SHIFT, 1, movetoworkspace, 1"
      "SUPER SHIFT, 2, movetoworkspace, 2"
      "SUPER SHIFT, 3, movetoworkspace, 3"
      "SUPER SHIFT, 4, movetoworkspace, 4"
      "SUPER SHIFT, 5, movetoworkspace, 5"
      "SUPER SHIFT, 6, movetoworkspace, 6"
      "SUPER SHIFT, 7, movetoworkspace, 7"
      "SUPER SHIFT, 8, movetoworkspace, 8"

      "SUPER, mouse_down, workspace, e+1"
      "SUPER, mouse_up, workspace, e-1"
    ];

    bindm = [
      "SUPER, mouse:272, movewindow"
      "SUPER, mouse:273, resizewindow"
    ];
  };
  wayland.windowManager.hyprland.extraConfig = ''
    # Some default env vars.
    env = XCURSOR_SIZE,24
  '';
}
