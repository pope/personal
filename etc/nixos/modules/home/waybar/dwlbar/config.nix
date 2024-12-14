{ config, pkgs }:

let
  color = config.my.home.theme.colors.withHash;
  pavucontrol = "${pkgs.pavucontrol}/bin/pavucontrol";
  wlogout = "${pkgs.wlogout}/bin/wlogout";
in
{
  dwlBar = {
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
    # Module s
    "custom/nixos" = {
      format = "";
      tooltip = false;
    };
    "dwl/window" = {
      format = "{title} <small>{layout}</small>";
      rewrite = {
        # When no window is present, show this message
        "^ <small>.*?</small>$" = ''<span foreground="${color.base04}">Get ready for a new challenger</span>'';
      };
    };
    mpris = {
      format = "{status_icon} {dynamic}";
      format-paused = "{status_icon} <i>{dynamic}</i>";
      dynamic-separator = ''<span foreground="${color.base04}"> ) </span>'';
      status-icons = {
        playing = "▶";
        paused = "⏸";
        stopped = "◼";
      };
    };
    pulseaudio = {
      # scroll-step = 1; # %, can be a float
      format = "{volume}% {icon} {format_source}";
      format-bluetooth = "{volume}% {icon} {format_source}";
      format-bluetooth-muted = " {icon} {format_source}";
      format-muted = " {format_source}";
      format-source = "{volume}% ";
      format-source-muted = "";
      format-icons = {
        headphone = "";
        hands-free = "";
        headset = "";
        phone = "";
        portable = "";
        car = "";
        default = [ "" "" "" ];
      };
      on-click = "${pavucontrol}";
    };
    cpu = {
      format = "{usage}% ";
      format-alt = "{avg_frequency} GHz ";
      interval = 10;
      tooltip = false;
    };
    memory = {
      format = "{}% ";
      format-alt = "{used}/{total} GiB ";
    };
    disk = {
      format = "{}% 󰋊";
      format-alt = "{used}/{total} GiB 󰋊";
      interval = 30;
      path = "/";
    };
    battery = {
      states = {
        good = 95;
        warning = 30;
        critical = 1;
      };
      format = "{capacity}% {icon}";
      format-full = "{capacity}% {icon}";
      format-charging = "{capacity}% ";
      format-plugged = "{capacity}% ";
      format-alt = "{time} {icon}";
      # format-good = ""; # An empty format will hide the module
      # format-full = "";
      format-icons = [ "" "" "" "" "" ];
    };
    "battery#bat1" = {
      bat = "BAT1";
    };
    power-profiles-daemon = {
      format = "{icon}";
      tooltip-format = "Power profile: {profile}\nDriver: {driver}";
      tooltip = true;
      format-icons = {
        default = "";
        performance = "";
        balanced = "";
        power-saver = "";
      };
    };
    idle_inhibitor = {
      format = "{icon}";
      format-icons = {
        activated = "";
        deactivated = "";
      };
    };
    tray = {
      icon-size = 16;
      spacing = 10;
    };
    clock = {
      tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
      format-alt = "{:%Y-%m-%d}";
    };
    "custom/power" = {
      format = "⏻ ";
      tooltip = false;
      on-click = "${wlogout}";
    };
  };
}
