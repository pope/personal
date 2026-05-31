{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.home.hyprland;

  inherit (lib.generators) mkLuaInline;
  inherit (lib) getExe;
  toLua' = lib.generators.toLua { };

  launcher = "${getExe pkgs.uwsm} app --";

  #color overrides
  inherit (config.my.home.theme) colorScheme;
  inherit (config.my.home.theme) colors;

  color_active_border_a = if colorScheme == "tokyonight" then colors.base0E else colors.base0B;
  color_active_border_b = if colorScheme == "tokyonight" then colors.base0F else colors.base0D;
in
{
  config = lib.mkIf cfg.enable {
    wayland.windowManager.hyprland.settings = with colors; {
      # Refer to https://wiki.hypr.land/Configuring/Basics/Variables/
      config = {
        # Look and feel
        general = {
          gaps_in = 5;
          gaps_out = 10;

          border_size = 2;

          col = {
            active_border = {
              colors = [
                "rgba(${color_active_border_a}ee)"
                "rgba(${color_active_border_b}ee)"
              ];
              angle = 45;
            };
            inactive_border = "rgba(${base02}aa)";
          };
        };
        decoration = {
          rounding = 10;
          rounding_power = 2;

          active_opacity = 1.0;
          inactive_opacity = 0.8;

          shadow = {
            enabled = !cfg.enableBatterySaverMode;
            range = 4;
            render_power = 3;
            color = "rgba(${base00}99)";
            # color = "0xee1a1a1a";
          };

          blur = {
            enabled = true;
            size = if cfg.enableBatterySaverMode then 8 else 3;
            passes = if cfg.enableBatterySaverMode then 1 else 4;
            vibrancy = 0.1696;
          };
        };
        animations.enabled = !cfg.enableBatterySaverMode;

        # See https://wiki.hypr.land/Configuring/Layouts/Dwindle-Layout/ for more
        dwindle.preserve_split = true;
        # See https://wiki.hypr.land/Configuring/Layouts/Master-Layout/ for more
        master.new_status = "master";
        # See https://wiki.hypr.land/Configuring/Layouts/Scrolling-Layout/ for more
        scrolling.fullscreen_on_one_column = true;

        # Misc
        misc = {
          force_default_wallpaper = -1;
          disable_hyprland_logo = false;
          vrr = cfg.enableVrr;
        };

        # Input
        input = {
          kb_layout = "us";
          kb_variant = "";
          kb_model = "";
          kb_options = "ctrl:nocaps";
          kb_rules = "";

          accel_profile = "flat";
          repeat_delay = 160;
          repeat_rate = 25;

          follow_mouse = 1;
          sensitivity = 0;
          touchpad = {
            natural_scroll = true;
            tap_to_click = false;
          };
        };
      };

      gesture = {
        fingers = 3;
        direction = "horizontal";
        action = "workspace";
      };
      # Can add per-device configs here.
      # See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Devices/ for more

      # See https://wiki.hypr.land/Configuring/Basics/Window-Rules/
      # and https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/
      window_rule = map (x: { _args = [ x ]; }) [
        {
          name = "suppress-maximize-events";
          match = {
            class = ".*";
          };
          suppress_event = "maximize";
        }
        {
          name = "fix-xwayland-drags";
          match = {
            class = "^$";
            title = "^$";
            xwayland = true;
            float = true;
            fullscreen = false;
            pin = false;
          };
          no_focus = true;
        }
        {
          match = {
            class = "^(Emacs)$";
          };
          opacity = "0.9 0.8";
        }
      ];
      layer_rule = map (x: { _args = [ x ]; }) [
        {
          name = "rofi-layer";
          match = {
            namespace = "rofi";
          };
          blur = true;
          dim_around = true;
          ignore_alpha = true;
        }
        {
          name = "bar-layer";
          match = {
            namespace = "^(gtk-layer-shell|anyrun|waybar)$";
          };
          blur = true;
          ignore_alpha = true;
        }
        {
          name = "notif-layer";
          match = {
            namespace = "notifications";
          };
          blur = true;
        }
        {
          name = "launcher-layer";
          match = {
            namespace = "launcher";
          };
          blur = true;
        }
      ];

      on =
        let
          nm-applet = "${launcher} ${getExe pkgs.networkmanagerapplet}";
          systemctl = "${pkgs.systemd}/bin/systemctl";
        in
        map (x: { _args = x; }) [
          [
            "hyprland.start"
            (mkLuaInline ''
              function ()
                hl.exec_cmd("${systemctl} --user start tile-manager-session.target")
                hl.exec_cmd("${nm-applet} --indicator")
              end'')
          ]
        ];

      bind =
        let
          inherit (config.programs)
            rofi
            anyrun
            ghostty
            wezterm
            kitty
            ;

          # commands
          brillo = getExe pkgs.brillo;
          playerctrl = getExe pkgs.playerctl;
          pkill = "${pkgs.procps}/bin/pkill";
          runner =
            if config.my.home.rofi.enable then
              "${launcher} ${getExe rofi.finalPackage} -show drun -run-command \"${launcher} {cmd}\""
            else
              "${launcher} ${getExe anyrun.package}";
          screenshot = "${launcher} wayland-screenshot";
          terminal =
            if config.my.home.terminals.ghostty.enable then
              "${launcher} ${getExe ghostty.package}"
            else if config.my.home.terminals.wezterm.enable then
              "${launcher} ${getExe wezterm.package}"
            else
              "${launcher} ${getExe kitty.package}";
          thunar = "${launcher} ${getExe pkgs.thunar}";
          wlogout = "${launcher} ${getExe pkgs.wlogout}";
          wpctl = "${pkgs.wireplumber}/bin/wpctl";

          execCmd = cmd: mkLuaInline "hl.dsp.exec_cmd(${toLua' cmd})";

          # Simple bind functions to keep line formatting in check
          b = k: c: [
            k
            c
          ];
          b' = k: c: a: [
            k
            c
            a
          ];
          workspace =
            num:
            let
              n = toString num;
            in
            b "SUPER + ${n}" (mkLuaInline "hl.dsp.focus({ workspace = ${n} })");
          workspaceMove =
            num:
            let
              n = toString num;
            in
            b "SUPER + ${n}" (mkLuaInline "hl.dsp.window.move({ workspace = ${n} })");
        in
        map (x: { _args = x; }) [
          (b "SUPER + Q" (mkLuaInline "hl.dsp.window.close()"))
          (b "SUPER + SHIFT + Escape" (mkLuaInline "hl.dsp.exit()"))

          (b "SUPER + F" (mkLuaInline "hl.dsp.window.fullscreen()"))
          (b "SUPER + V" (mkLuaInline "hl.dsp.window.float({ action = 'toggle' })"))
          (b "SUPER + P" (mkLuaInline "hl.dsp.window.pseudo()"))
          (b "SUPER + J" (mkLuaInline "hl.dsp.layout('togglesplit')")) # dwindle only
          (b "SUPER + C" (mkLuaInline "hl.dsp.window.center()"))

          (b "SUPER + B" (execCmd "${pkill} -SIGUSR1 waybar"))

          (b "SUPER + Return" (execCmd terminal))
          (b "SUPER + E" (execCmd thunar))
          (b "SUPER + Space" (execCmd runner))
          (b "SUPER + L" (execCmd wlogout))

          (b "SUPER + left" (mkLuaInline "hl.dsp.focus({ direction = 'left' })"))
          (b "SUPER + right" (mkLuaInline "hl.dsp.focus({ direction = 'right' })"))
          (b "SUPER + up" (mkLuaInline "hl.dsp.focus({ direction = 'up' })"))
          (b "SUPER + down" (mkLuaInline "hl.dsp.focus({ direction = 'down' })"))

          (b "SUPER + SHIFT + left" (mkLuaInline "hl.dsp.window.move({ direction = 'left' })"))
          (b "SUPER + SHIFT + right" (mkLuaInline "hl.dsp.window.move({ direction = 'right' })"))
          (b "SUPER + SHIFT + up" (mkLuaInline "hl.dsp.window.move({ direction = 'up' })"))
          (b "SUPER + SHIFT + down" (mkLuaInline "hl.dsp.window.move({ direction = 'down' })"))

          (b "SUPER + ALT + left" (mkLuaInline "hl.dsp.window.resize({ x = 50, y = 0 })"))
          (b "SUPER + ALT + right" (mkLuaInline "hl.dsp.window.resize({ x = -50, y = 0 })"))
          (b "SUPER + ALT + up" (mkLuaInline "hl.dsp.window.resize({ x = 0, y = -50 })"))
          (b "SUPER + ALT + down" (mkLuaInline "hl.dsp.window.resize({ x = 0, y = 50 })"))

          (b "SUPER + Tab" (mkLuaInline ''
            function()
              hl.dispatch(hl.dsp.window.cycle_next())
              hl.dispatch(hl.dsp.window.bring_to_top())
            end''))

          (b "SUPER + A" (mkLuaInline "hl.dsp.workspace.toggle_special('magic')"))
          (b "SUPER + SHIFT + A" (mkLuaInline "hl.dsp.window.move({ workspace = 'special:magic' })"))

          (workspace 1)
          (workspace 2)
          (workspace 3)
          (workspace 4)
          (workspace 5)
          (workspace 6)
          (workspace 7)
          (workspace 8)
          (workspace 9)
          (workspace 0)

          (workspaceMove 1)
          (workspaceMove 2)
          (workspaceMove 3)
          (workspaceMove 4)
          (workspaceMove 5)
          (workspaceMove 6)
          (workspaceMove 7)
          (workspaceMove 8)
          (workspaceMove 9)
          (workspaceMove 0)

          (b "SUPER + mouse_down" (mkLuaInline "hl.dsp.focus({ workspace = 'e+1' })"))
          (b "SUPER + mouse_up" (mkLuaInline "hl.dsp.focus({ workspace = 'e-1' })"))

          (b' "SUPER + mouse:272" (mkLuaInline "hl.dsp.window.drag()") { mouse = true; })
          (b' "SUPER + mouse:273" (mkLuaInline "hl.dsp.window.resize()") { mouse = true; })

          # Monitor Brightness
          (b' "XF86MonBrightnessUp" (execCmd "${brillo} -A 5") {
            locked = true;
            repeating = true;
          })
          (b' "XF86MonBrightnessDown" (execCmd "${brillo} -U 5") {
            locked = true;
            repeating = true;
          })
          # Autio levels
          (b' "XF86AudioRaiseVolume" (execCmd "${wpctl} set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+") {
            locked = true;
            repeating = true;
          })
          (b' "XF86AudioLowerVolume" (execCmd "${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%-") {
            locked = true;
            repeating = true;
          })
          (b' "XF86AudioMute" (execCmd "${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle") {
            locked = true;
            repeating = true;
          })
          (b' "XF86AudioMicMute" (execCmd "${wpctl} set-mute @DEFAULT_AUDIO_SOURCE@ toggle") {
            locked = true;
            repeating = true;
          })
          # Screenshot
          (b' "Print" (execCmd "${screenshot}") { locked = true; })
          # Audio Playback
          (b' "XF86AudioNext" (execCmd "${playerctrl} next") { locked = true; })
          (b' "XF86AudioPause" (execCmd "${playerctrl} play-pause") { locked = true; })
          (b' "XF86AudioPlay" (execCmd "${playerctrl} play-pause") { locked = true; })
          (b' "XF86AudioPrev" (execCmd "${playerctrl} previous") { locked = true; })
        ];

      # Default curves and animations, see https://wiki.hypr.land/Configuring/Advanced-and-Cool/Animations/
      curve = map (x: { _args = x; }) [
        [
          "easeOutQuint"
          {
            type = "bezier";
            points = [
              [
                0.23
                1
              ]
              [
                0.32
                1
              ]
            ];
          }
        ]
        [
          "easeInOutCubic"
          {
            type = "bezier";
            points = [
              [
                0.65
                0.05
              ]
              [
                0.36
                1
              ]
            ];
          }
        ]
        [
          "linear"
          {
            type = "bezier";
            points = [
              [
                0
                0
              ]
              [
                1
                1
              ]
            ];
          }
        ]
        [
          "almostLinear"
          {
            type = "bezier";
            points = [
              [
                0.5
                0.5
              ]
              [
                0.75
                1
              ]
            ];
          }
        ]
        [
          "quick"
          {
            type = "bezier";
            points = [
              [
                0.15
                0
              ]
              [
                0.1
                1
              ]
            ];
          }
        ]
        # Default springs
        [
          "easy"
          {
            type = "spring";
            mass = 1;
            stiffness = 71.2633;
            dampening = 15.8273644;
          }
        ]
      ];
      animation = [
        {
          leaf = "global";
          enabled = true;
          speed = 10;
          bezier = "default";
        }
        {
          leaf = "border";
          enabled = true;
          speed = 5.39;
          bezier = "easeOutQuint";
        }
        {
          leaf = "windows";
          enabled = true;
          speed = 4.79;
          spring = "easy";
        }
        {
          leaf = "windowsIn";
          enabled = true;
          speed = 4.1;
          spring = "easy";
          style = "popin 87%";
        }
        {
          leaf = "windowsOut";
          enabled = true;
          speed = 1.49;
          bezier = "linear";
          style = "popin 87%";
        }
        {
          leaf = "fadeIn";
          enabled = true;
          speed = 1.73;
          bezier = "almostLinear";
        }
        {
          leaf = "fadeOut";
          enabled = true;
          speed = 1.46;
          bezier = "almostLinear";
        }
        {
          leaf = "fade";
          enabled = true;
          speed = 3.03;
          bezier = "quick";
        }
        {
          leaf = "layers";
          enabled = true;
          speed = 3.81;
          bezier = "easeOutQuint";
        }
        {
          leaf = "layersIn";
          enabled = true;
          speed = 4;
          bezier = "easeOutQuint";
          style = "fade";
        }
        {
          leaf = "layersOut";
          enabled = true;
          speed = 1.5;
          bezier = "linear";
          style = "fade";
        }
        {
          leaf = "fadeLayersIn";
          enabled = true;
          speed = 1.79;
          bezier = "almostLinear";
        }
        {
          leaf = "fadeLayersOut";
          enabled = true;
          speed = 1.39;
          bezier = "almostLinear";
        }
        {
          leaf = "workspaces";
          enabled = true;
          speed = 1.94;
          bezier = "almostLinear";
          style = "fade";
        }
        {
          leaf = "workspacesIn";
          enabled = true;
          speed = 1.21;
          bezier = "almostLinear";
          style = "fade";
        }
        {
          leaf = "workspacesOut";
          enabled = true;
          speed = 1.94;
          bezier = "almostLinear";
          style = "fade";
        }
        {
          leaf = "zoomFactor";
          enabled = true;
          speed = 7;
          bezier = "quick";
        }
      ];
    };
  };
}
