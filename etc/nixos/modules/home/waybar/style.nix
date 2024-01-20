{ config }:

let
  color = config.my.home.theme.colors.withHash;
in
''
  * {
    font-size: 0.9rem;
  }

  window#waybar {
    background: alpha(${color.base00}, 0.85);
    color: ${color.base05};
    transition-duration: .5s;
    transition-property: background-color;
  }

  .modules-left {
    background-color: alpha(${color.base01}, 0.5);
    border-right: 3px solid ${color.base02};
    border-radius: 0 16px 16px 0;
    padding-right: 1rem;
  }

  .modules-left > :first-child {
    background-color: ${color.base0D};
    border-radius: 0 16px 16px 0;
    border-right: 3px solid ${color.base05};
  }

  #custom-launcher {
    color: ${color.base00};
    margin-left: 0.5rem;
    margin-right: 1rem;
  }

  #custom-playerctl {
    margin-right: 0.5rem;
    margin-left: 0.5rem;
  }

  #workspaces {
    background: alpha(${color.base01}, 0.5);
    border-radius: 16px;
    font-style: normal;
    font-weight: normal;
    margin: 3px;
    padding: 5px 0.5rem;
  }

  #workspaces button {
    background-color: ${color.base02};
    border-radius: 16px;
    color: ${color.base02};
    margin: 0px 3px;
    min-height: 10px;
    padding: 0px 5px;
    transition: all 0.3s ease-in-out;
  }

  #workspaces button.active {
    background-color: ${color.base0D};
    background-size: 400% 400%;
    border-radius: 16px;
    color: ${color.base0D};
    min-width: 50px;
    transition: all 0.3s ease-in-out;
  }

  #workspaces button *:hover {
    background: none;
    border: none;
  }

  #workspaces button:hover {
    background: ${color.base0A};
    background-size: 400% 400%;
    border-radius: 16px;
    color: ${color.base0A};
    min-width: 50px;
  }

  tooltip {
    background: ${color.base00};
    border-color: ${color.base00};
    border-radius: 10px;
    border-style: solid;
    border-width: 2px;
  }

  .modules-right {
    background-color: alpha(${color.base01}, 0.5);
    border-left: 3px solid ${color.base02};
    border-radius: 16px 0 0 16px;
    padding-left: 1rem;
  }

  .modules-right > :last-child {
    background-color: ${color.base0D};
    border-radius: 16px 0 0 16px;
    border-left: 3px solid ${color.base05};
  }

  #custom-power {
    color: ${color.base00};
    margin-right: 0.5rem;
    margin-left: 1.2rem;
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

  #pulseaudio,
  #tray {
    border-left: 3px solid ${color.base04};
    border-radius: 16px 0 0 16px;
    padding-left: 1rem;
  }

  #tray {
    border-left: 3px solid ${color.base03};
    margin-right: 0.3rem;
  }
  #tray>.passive {
    -gtk-icon-effect: dim;
  }
  #tray>.needs-attention {
    -gtk-icon-effect: highlight;
  }

  #cpu, #battery {
    color: ${color.base0C};
  }

  #battery {
    margin-right: 1rem;
  }

  #battery.warning {
    color: ${color.base09};
  }

  #battery.critical {
    color: ${color.base08};
  }

  #memory, #clock {
    color: ${color.base0D};
  }

  #disk, #pulseaudio {
    color: ${color.base0B};
  }
''
