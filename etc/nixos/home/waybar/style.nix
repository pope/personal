{ config }:

let
  color = config.colorScheme.colors;
in
''
  * {
    font-size: 0.9rem;
  }

  window#waybar {
    background: rgba(0, 0, 0, 0.85);
    color: #${color.base05};
    transition-duration: .5s;
    transition-property: background-color;
  }

  .modules-left,
  .modules-right,
  .modules-center {}

  #custom-launcher {
    color: #${color.base0C};
    margin-left: 1rem;
    margin-right: 1.2rem;
  }

  #custom-playerctl {
    margin-right: 0.5rem;
  }

  tooltip {
    background: #${color.base00};
    border-color: #${color.base00};
    border-radius: 10px;
    border-style: solid;
    border-width: 2px;
  }

  #workspaces {
    background: #${color.base00};
    border-radius: 16px;
    border: solid 0px #${color.base05};
    font-style: normal;
    font-weight: normal;
    margin: 5px 5px;
    padding: 8px 5px;
  }

  #workspaces button {
    background-color: #${color.base02};
    border-radius: 16px;
    color: #${color.base02};
    margin: 0px 3px;
    padding: 0px 5px;
    transition: all 0.3s ease-in-out;
  }

  #workspaces button.active {
    background-color: #${color.base0D};
    background-size: 400% 400%;
    border-radius: 16px;
    color: #${color.base0D};
    min-width: 50px;
    transition: all 0.3s ease-in-out;
  }

  #workspaces button:hover {
    background-color: #${color.base0A};
    background-size: 400% 400%;
    border-radius: 16px;
    color: #${color.base0A};
    min-width: 50px;
  }

  #cpu,
  #memory,
  #disk,
  #tray,
  #pulseaudio,
  #clock,
  #battery {
    margin-left: 0.6rem;
    margin-right: 0.6rem;
  }

  #tray>.passive {
    -gtk-icon-effect: dim;
  }

  #tray>.needs-attention {
    -gtk-icon-effect: highlight;
  }

  #cpu, #battery {
    color: #${color.base0C};
  }

  #battery.warning {
    color: #${color.base09};
  }

  #battery.critical {
    color: #${color.base08};
  }

  #memory, #clock {
    color: #${color.base0D};
  }

  #disk, #pulseaudio {
    color: #${color.base0B};
  }
''
