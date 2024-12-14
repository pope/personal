{ config, pkgs, hyprland }:

let
  color = config.my.home.theme.colors.withHash;
  hyprctl = "${hyprland}/bin/hyprctl";
  playerctl = "${pkgs.playerctl}/bin/playerctl";
  nm-connection-editor = "${pkgs.networkmanagerapplet}/bin/nm-connection-editor";
  pavucontrol = "${pkgs.pavucontrol}/bin/pavucontrol";
  wlogout = "${pkgs.wlogout}/bin/wlogout";
in
{
  mainBar = {
    position = "top";
    layer = "top";
    height = 24;
    margin-top = 0;
    margin-bottom = 0;
    margin-left = 0;
    margin-right = 0;
    modules-left = [
      "custom/launcher"
      "custom/playerctl"
      "custom/playerlabel"
    ];
    modules-center = [
      "hyprland/workspaces"
    ];
    modules-right = [
      "cpu"
      "memory"
      "disk"
      "tray"
      # "network"
      "pulseaudio"
      "clock"
      "battery"
      "custom/power"
    ];

    clock = {
      format = "󱑍 {:%H:%M}";
      tooltip = "true";
      tooltip-format = ''
        <big>{:%Y %B}</big>
        <tt><small>{calendar}</small></tt>'';
      format-alt = " {:%m/%d}";
    };

    "hyprland/workspaces" = {
      active-only = false;
      all-outputs = false;
      disable-scroll = false;
      on-scroll-up = "${hyprctl} dispatch workspace e-1";
      on-scroll-down = "${hyprctl} dispatch workspace e+1";
      format = " ";
      on-click = "activate";
      show-special = "false";
      sort-by-number = true;
      persistent-workspaces = {
        "*" = 8;
      };
    };

    "custom/playerctl" = {
      format = "{icon}";
      return-type = "json";
      exec = ''
        ${playerctl} -a metadata --format '{"text": "{{artist}} - {{markup_escape(title)}}", "tooltip": "{{playerName}} : {{markup_escape(title)}}", "alt": "{{status}}", "class": "{{status}}"}' -F
      '';
      on-click-middle = "${playerctl} play-pause";
      on-click = "${playerctl} previous";
      on-click-right = "${playerctl} next";
      format-icons = {
        Playing = "<span foreground='${color.base0B}'>󰓇 </span>";
        Paused = "<span foreground='${color.base0E}'>󰓇 </span>";
      };
    };

    "custom/playerlabel" = {
      format = "<span>{}</span>";
      return-type = "json";
      max-length = 75;
      exec = ''
        ${playerctl} -a metadata --format '{"text": "{{artist}} - {{markup_escape(title)}}", "tooltip": "{{playerName}} : {{markup_escape(title)}}", "alt": "{{status}}", "class": "{{status}}"}' -F
      '';
      on-click-middle = "${playerctl} play-pause";
      on-click = "${playerctl} previous";
      on-click-right = "${playerctl} next";
    };

    battery = {
      states = {
        good = 95;
        warning = 30;
        critical = 15;
      };
      format = "{icon}  {capacity}%";
      format-charging = "{capacity}% 󱐋";
      format-plugged = "{capacity}%  ";
      format-alt = "{icon} {time}";
      # "format-good"= "", # An empty format will hide the module
      # "format-full"= "";
      format-icons = [ " " " " " " " " " " ];
    };

    memory = {
      format = "󰍛 {}%";
      format-alt = "󰍛 {used}/{total} GiB";
      interval = 30;
    };

    cpu = {
      format = "󰻠 {usage}%";
      format-alt = "󰻠 {avg_frequency} GHz";
      interval = 10;
    };

    disk = {
      format = "󰋊 {}%";
      format-alt = "󰋊 {used}/{total} GiB";
      interval = 30;
      path = "/";
    };

    network = {
      format-wifi = "󰤨";
      format-ethernet = " {ifname}: Aesthetic";
      format-linked = " {ifname} (No IP)";
      format-disconnected = "󰤭 ";
      format-alt = " {ifname}: {ipaddr}/{cidr}";
      tooltip-format = "{essid}";
      on-click-right = nm-connection-editor;
    };

    tray = {
      icon-size = 12;
      spacing = 10;
    };

    pulseaudio = {
      format = "{icon} {volume}%";
      format-muted = "";
      format-icons = { default = [ "" "" "" ]; };
      scroll-step = 1;
      on-click-right = pavucontrol;
      tooltip = false;
    };

    "custom/launcher" = {
      format = "";
      # on-click = "notify-send -t 1 'swww' '1' & ~/.config/hypr/scripts/wall";
      tooltip = false;
    };

    "custom/power" = {
      format = "⏻";
      on-click = wlogout;
      tooltip = false;
    };
  };
  dwlBar = {
    id = "dwl-bar";
    name = "dwl-bar";
    layer = "top";
    position = "top";
    height = 36;
    spacing = 4;
    modules-left = [
      "custom/nixos"
      "dwl/tags"
      "dwl/window"
    ];
    modules-center = [
      "mpris"
    ];
    modules-right = [
      "pulseaudio"
      "cpu"
      "memory"
      "disk"
      "battery"
      # "battery#bat1"
      "power-profiles-daemon"
      "idle_inhibitor"
      "tray"
      "clock"
      "custom/power"
    ];
    # Modules
    "custom/nixos" = {
      format = "";
      tooltip = false;
    };
    "dwl/window" = {
      format = ''
        {title} <small>{layout}</small>
      '';
      # TODO(pope): Dynamically update the foreground color
      rewrite = {
        "^ <small>.*?</small>$" = ''
          <span foreground="#585b70">Get ready for a new challenger</span>
        '';
      };
    };
  };
}
