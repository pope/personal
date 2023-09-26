{ config }:

let
  color = config.colorScheme.colors;
in
{
  mainBar = {
    position = "top";
    layer = "top";
    height = 42;
    margin-top = 0;
    margin-bottom = 0;
    margin-left = 0;
    margin-right = 0;
    modules-left = [ "custom/launcher" "custom/playerctl" "custom/playerlabel" ];
    modules-center = [
      "hyprland/workspaces"
    ];
    modules-right = [
      "cpu"
      "memory"
      "disk"
      "tray"
      "pulseaudio"
      "clock"
      # "network"
    ];
    clock = {
      format = "󱑍 {:%H:%M}";
      tooltip = "true";
      tooltip-format = ''
        <big>{:%Y %B}</big>
        <tt><small>{calendar}</small></tt>'';
      format-alt = " {:%d/%m}";
    };

    "hyprland/workspaces" = {
      active-only = false;
      all-outputs = false;
      disable-scroll = false;
      on-scroll-up = "hyprctl dispatch workspace e-1";
      on-scroll-down = "hyprctl dispatch workspace e+1";
      format = "{icon}";
      on-click = "activate";
      show-special = "false";
      sort-by-number = true;
      persistent_workspaces = {
        "*" = 8;
      };
    };

    "custom/playerctl" = {
      format = "{icon}";
      return-type = "json";
      exec = ''
        playerctl -a metadata --format '{"text": "{{artist}} - {{markup_escape(title)}}", "tooltip": "{{playerName}} : {{markup_escape(title)}}", "alt": "{{status}}", "class": "{{status}}"}' -F'';
      on-click-middle = "playerctl play-pause";
      on-click = "playerctl previous";
      on-click-right = "playerctl next";
      format-icons = {
        Playing = "<span foreground='#${color.base09}'>󰓇 </span>";
        Paused = "<span foreground='#${color.base08}'>󰓇 </span>";
      };
    };

    "custom/playerlabel" = {
      format = "<span>{}</span>";
      return-type = "json";
      max-length = 35;
      exec = ''
        playerctl -a metadata --format '{"text": "{{artist}} - {{markup_escape(title)}}", "tooltip": "{{playerName}} : {{markup_escape(title)}}", "alt": "{{status}}", "class": "{{status}}"}' -F'';
      on-click-middle = "playerctl play-pause";
      on-click = "playerctl previous";
      on-click-right = "playerctl next";
    };

    battery = {
      states = {
        good = 95;
        warning = 30;
        critical = 15;
      };
      format = "{icon}  {capacity}%";
      format-charging = "{capacity}% ";
      format-plugged = "{capacity}% ";
      format-alt = "{icon} {time}";
      # "format-good"= "", # An empty format will hide the module
      # "format-full"= "";
      format-icons = [ "" "" "" "" "" ];
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
      format-disconnected = "󰤭";
      format-alt = " {ifname}: {ipaddr}/{cidr}";
      tooltip-format = "{essid}";
      on-click-right = "nm-connection-editor";
    };

    tray = {
      icon-size = 18;
      spacing = 10;
    };

    pulseaudio = {
      format = "{icon} {volume}%";
      format-muted = "";
      format-icons = { default = [ "" "" "" ]; };
      scroll-step = 1;
      on-click-right = "pavucontrol";
      tooltip = false;
    };

    "custom/launcher" = {
      format = "";
      # on-click = "notify-send -t 1 'swww' '1' & ~/.config/hypr/scripts/wall";
      tooltip = false;
    };
  };
}