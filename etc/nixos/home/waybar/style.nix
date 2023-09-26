{ config }:

let
  color = config.colorScheme.colors;
in
''
  * {
    border: none;
    border-radius: 0px;
    /* font-family: LigaSFMono Nerd Font, Iosevka, FontAwesome, Noto Sans CJK; */
    font-family: Iosevka, FontAwesome, Noto Sans CJK;
    /* font-family: JetBrainsMono Nerd Font, FontAwesome, Noto Sans CJK; */
    font-size: 12px;
    font-style: normal;
    min-height: 0;
  }

  window#waybar {
    background: rgba(0, 0, 0, 0.85);
    color: #${color.base05};
    transition-duration: .5s;
    transition-property: background-color;
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
    background-color: #${color.base0A};
    border-radius: 16px;
    color: #${color.base0A};
    margin: 0px 3px;
    padding: 0px 5px;
    transition: all 0.3s ease-in-out;
  }

  #workspaces button.active {
    background-color: #${color.base05};
    background-size: 400% 400%;
    border-radius: 16px;
    color: #${color.base05};
    min-width: 50px;
    transition: all 0.3s ease-in-out;
  }

  #workspaces button:hover {
    background-color: #${color.base05};
    background-size: 400% 400%;
    border-radius: 16px;
    color: #${color.base05};
    min-width: 50px;
  }

  #custom-date, #custom-playerctl, #clock, #battery, #pulseaudio, #network {
    background: #${color.base00};
    border-radius: 10px;
    color: #${color.base05};
    margin: 6px;
    padding: 0 6px;
  }

  #pulseaudio, #tray, #clock, #custom-launcher {
    margin-right: 6px;
  }

  #cpu,
  #disk,
  #memory,
  #pulseaudio,
  #backlight,
  #battery,
  #network,
  #clock {
      border-radius: 0px 10px 10px 0px;
      margin: 6px;
  }

  #tray {
    color: #${color.base0E};
  }

  #tray>.passive {
    -gtk-icon-effect: dim;
  }

  #tray>.needs-attention {
    -gtk-icon-effect: highlight;
  }

  custom-launcher {
    border-radius: 0px 10px 0px 0px;
    color: #${color.base0C};
    font-size: 20px;
    margin: 0px 0px 2px 0px;
    padding: 10px 15px 10px 15px;
  }

  #pulseaudio {
    color: #${color.base05};
  }

  #pulseaudio.muted {
    color: #${color.base01};
  }
''
