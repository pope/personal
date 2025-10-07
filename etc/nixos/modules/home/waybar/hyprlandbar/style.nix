{ config }:

let
  color = config.my.home.theme.colors.withHash;
in
# css
''
  @define-color base00 ${color.base00};
  @define-color base01 ${color.base01};
  @define-color base02 ${color.base02};
  @define-color base03 ${color.base03};
  @define-color base04 ${color.base04};
  @define-color base05 ${color.base05};
  @define-color base06 ${color.base06};
  @define-color base07 ${color.base07};
  @define-color base08 ${color.base08};
  @define-color base09 ${color.base09};
  @define-color base0A ${color.base0A};
  @define-color base0B ${color.base0B};
  @define-color base0C ${color.base0C};
  @define-color base0D ${color.base0D};
  @define-color base0E ${color.base0E};
  @define-color base0F ${color.base0F};

  * {
    font-family: sans-serif, "Symbols Nerd Font", monospace;
    font-weight: normal;
    font-size: 0.8rem;
  }

  window#waybar {
    background: alpha(@base00, 0.85);
    color: @base05;
    transition-duration: .5s;
    transition-property: background-color;
  }

  .modules-left {
    background-color: alpha(@base01, 0.5);
    border-right: 3px solid @base02;
    border-radius: 0 16px 16px 0;
    padding-right: 1rem;
  }

  .modules-left > :first-child {
    background-color: @base0D;
    border-radius: 0 16px 16px 0;
    border-right: 3px solid @base05;
  }

  #custom-launcher {
    color: @base00;
    font-size: 1rem;
    margin-left: 0.5rem;
    margin-right: 1rem;
  }

  #mpris {
    margin-right: 0.5rem;
    margin-left: 0.5rem;
  }

  #workspaces {
    background: alpha(@base01, 0.5);
    border-radius: 16px;
    font-style: normal;
    font-weight: normal;
    margin: 3px;
    padding: 5px 0.5rem;
  }

  #workspaces button {
    background-color: @base02;
    border-radius: 16px;
    color: @base02;
    margin: 0px 3px;
    min-height: 16px;
    min-width: 16px;
    padding: 0;
    transition: all 0.3s ease-in-out;
  }

  #workspaces button.active {
    background-color: @base0D;
    background-size: 400% 400%;
    border-radius: 16px;
    color: @base0D;
    min-width: 50px;
    min-height: 16px;
    transition: all 0.3s ease-in-out;
  }

  #workspaces button *:hover {
    background: none;
    border: none;
  }

  #workspaces button:hover {
    background: @base0A;
    background-size: 400% 400%;
    border-radius: 16px;
    color: @base0A;
    min-width: 50px;
    min-height: 16px;
  }

  tooltip {
    background: @base00;
    border-color: @base00;
    border-radius: 10px;
    border-style: solid;
    border-width: 2px;
  }

  .modules-right {
    background-color: alpha(@base01, 0.5);
    border-left: 3px solid @base02;
    border-radius: 16px 0 0 16px;
    padding-left: 1rem;
  }

  .modules-right > :last-child {
    background-color: @base0D;
    border-radius: 16px 0 0 16px;
    border-left: 3px solid @base05;
  }

  #custom-power {
    color: @base00;
    font-size: 1rem;
    margin-right: 0.5rem;
    margin-left: 1.2rem;
  }

  #cpu,
  #memory,
  #disk,
  #idle_inhibitor,
  #power-profiles-daemon,
  #tray,
  #pulseaudio,
  #clock,
  #battery {
    margin-left: 0.6rem;
    margin-right: 0.6rem;
  }

  #pulseaudio,
  #tray {
    border-left: 3px solid @base04;
    border-radius: 16px 0 0 16px;
    padding-left: 1rem;
  }

  #tray {
    border-left: 3px solid @base03;
    margin-right: 0.3rem;
  }
  #tray>.passive {
    -gtk-icon-effect: dim;
  }
  #tray>.needs-attention {
    -gtk-icon-effect: highlight;
  }

  #cpu, #battery {
    color: @base0C;
  }

  #battery {
    margin-right: 1rem;
  }

  #battery.warning {
    color: @base09;
  }

  #battery.critical {
    color: @base08;
  }

  #memory, #clock {
    color: @base0D;
  }

  #disk, #pulseaudio {
    color: @base0B;
  }
''
