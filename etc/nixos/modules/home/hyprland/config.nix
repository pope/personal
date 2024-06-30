{ config, pkgs, lib, inputs, ... }:

let
  inherit (lib) mkIf;
  inherit (inputs.hyprland.packages.${pkgs.system}) hyprland;
  anyrunPkg = inputs.anyrun.packages.${pkgs.system}.default;

  bg = "23.png";

  # commands
  _1password = "${pkgs._1password-gui}/bin/1password";
  anyrun = "${anyrunPkg}/bin/anyrun";
  dbus-update-activation-environment = "${pkgs.dbus}/bin/dbus-update-activation-environment";
  grim = "${pkgs.grim}/bin/grim";
  hyprctl = "${hyprland}/bin/hyprctl";
  light = "${pkgs.light}/bin/light";
  nm-applet = "${pkgs.networkmanagerapplet}/bin/nm-applet";
  pamixer = "${pkgs.pamixer}/bin/pamixer";
  slurp = "${pkgs.slurp}/bin/slurp";
  swappy = "${pkgs.swappy}/bin/swappy";
  swww = "${pkgs.swww}/bin/swww";
  systemctl = "${pkgs.systemd}/bin/systemctl";
  terminal = "${pkgs.wezterm}/bin/wezterm";
  thunar = "${pkgs.xfce.thunar}/bin/thunar";
  udiskie = "${pkgs.udiskie}/bin/udiskie";
  waybar = "${pkgs.waybar}/bin/waybar";
  polkit = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
in
{
  config = mkIf config.my.home.hyprland.enable {
    wayland.windowManager.hyprland.settings = with config.my.home.theme.colors; {
      monitor = lib.mkDefault [
        "eDP-1,preferred,auto,1"
      ];
      exec-once = [
        "${dbus-update-activation-environment} --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
        "${systemctl} --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
        polkit
        waybar
        "${swww} init --no-daemon"
        "${nm-applet} --indicator"
        "${udiskie} --appindicator --no-password-prompt"
        "${_1password} --silent"
      ];
      exec = [
        "sleep 3 && ${swww} img ${config.xdg.userDirs.pictures}/${bg}"
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
          natural_scroll = true;
          tap-to-click = false;
        };
      };

      general = {
        gaps_in = 5;
        gaps_out = 5;
        border_size = 2;
        "col.active_border" = "rgb(${base0B}) rgb(${base0D}) 45deg";
        "col.inactive_border" = "rgb(${base02})";
        layout = "dwindle";
      };

      cursor = {
        no_warps = true;
      };

      decoration = {
        rounding = 6;
        blur = {
          enabled = true;

          size = 6;
          passes = 3;
          new_optimizations = true;
          ignore_opacity = true;
          noise = "0.1";
          contrast = "1.1";
          brightness = "1.2";
          xray = false;
        };

        drop_shadow = true;
        shadow_ignore_window = true;
        shadow_offset = "0 8";
        shadow_range = 50;
        shadow_render_power = 3;
        "col.shadow" = "rgba(${base00}99)";
      };

      animations = {
        enabled = true;
        bezier = [
          "wind, 0.05, 0.9, 0.1, 1.05"
          "winIn, 0.1, 1.1, 0.1, 1.1"
          "winOut, 0.3, -0.3, 0, 1"
          "liner, 1, 1, 1, 1"
        ];
        animation = [
          "windows, 1, 6, wind, slide"
          "windowsIn, 1, 6, winIn, slide"
          "windowsOut, 1, 5, winOut, slide"
          "windowsMove, 1, 5, wind, slide"
          "border, 1, 1, liner"
          "borderangle, 1, 30, liner, loop"
          "fade, 1, 10, default"
          "workspaces, 1, 5, wind"
        ];
      };

      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;

        vfr = true; # misc:no_vfr -> misc:vfr. bool, heavily recommended to leave at default on. Saves on CPU usage.
        vrr = false; # misc:vrr -> Adaptive sync of your monitor. 0 (off), 1 (on), 2 (fullscreen only). Default 0 to avoid white flashes on select hardware.
      };

      group = {
        "col.border_active" = "rgb(${base0A})";
        "col.border_inactive" = "rgb(${base03})";

        groupbar = {
          font_size = 12;
          gradients = false;
        };
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
        new_status = "master";
      };

      gestures = {
        workspace_swipe = false;
      };

      bind = [
        "SUPER, Q, killactive"
        "SUPER SHIFT, Escape, exit"

        "SUPER, F, fullscreen"
        "SUPER, V, togglefloating"
        "SUPER, P, pseudo" # dwindle
        "SUPER, J, togglesplit" # dwindle
        "SUPER, G, togglegroup"
        "SUPER SHIFT, N, changegroupactive, f"
        "SUPER SHIFT, P, changegroupactive, b"

        "SUPER, Return, exec, ${terminal}"
        "SUPER, E, exec, ${thunar}"
        "SUPER, Space, exec, ${anyrun}"

        "SUPER, Tab, cyclenext"
        "SUPER, Tab, bringactivetotop"

        "SUPER, A, togglespecialworkspace"
        "SUPER SHIFT, A, movetoworkspace, special"
        "SUPER, C, exec, ${hyprctl} dispatch centerwindow"

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

        # Brightness Control Bindings
        ", XF86MonBrightnessUp, exec, ${light} -A 10"
        ", XF86MonBrightnessDown, exec, ${light} -U 10"
        # Audio levels
        ", XF86AudioRaiseVolume, exec, ${pamixer} -u -i 10"
        ", XF86AudioLowerVolume, exec, ${pamixer} -u -d 10"
        ", XF86AudioMute, exec, ${pamixer} -t"
        # Screenshot
        ", Print, exec, ${grim} -g \"$(${slurp})\" - | ${swappy} -f -"
      ];

      bindm = [
        "SUPER, mouse:272, movewindow"
        "SUPER, mouse:273, resizewindow"
      ];

      windowrulev2 = [
        "opacity 0.80 0.70,class:^(pavucontrol)$"
        "opacity 0.90 0.80,class:^(discord)$"
        "opacity 0.80 0.80,class:^(thunar)$"

        "float,class:^(pavucontrol)$"
      ];
      layerrule = [
        "blur, ^(gtk-layer-shell|anyrun|waybar)$"
        "ignorezero, ^(gtk-layer-shell|anyrun|waybar)$"
        "blur, notifications"
        "blur, launcher"
      ];
    };
    wayland.windowManager.hyprland.extraConfig = ''
      # Some default env vars.
      env = XCURSOR_SIZE,24
    '';
  };
}
