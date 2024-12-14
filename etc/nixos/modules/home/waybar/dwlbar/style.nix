{ config }:

let
  color = config.my.home.theme.colors.withHash;
in
  /* css */ ''
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
    font-family: monospace;
    font-size: 1rem;
    font-weight: normal;
  }

  window#waybar {
    background: alpha(@base00, 0.0);
    color: @base05;
  }

  .modules-left, .modules-right, #mpris {
    background: alpha(@base01, 0.90);
    border: 2px solid @base0E;
    border-radius: 16px;
    padding-left: 4px;
    padding-right: 4px;
  }
  .modules-left {
    margin-left: 8px;
  }
  .modules-right {
    margin-right: 8px;
  }

  #mpris {
    margin-left: 16px;
    margin-right: 16px;
  }

  #custom-nixos {
    margin-left: 8px;
    margin-right: 8px;
  }

  #tags button {
    border: 0px solid black;
    border-radius: 8px;
    box-shadow: none;
    color: @base03;
    min-width: 0px;
    padding: 4px 8px;
  }
  #tags button.focused {
    background-color: @base02;
  }
  #tags button:hover {
    background-color: @base02;
    background-image: none;
    border: 0px solid black;
    border-radius: 8px;
    color: @base04;
    box-shadow: none;
    text-shadow: none;
    -gtk-icon-effect: none;
    -gtk-icon-shadow: none;
    -gtk-icon-source: builtin;
  }

  #tags button label {
    font-weight: lighter;
  }
  #tags button.occupied label {
    color: @base05;
    font-weight: lighter;
  }
  #tags button.focused label {
    color: @base0E;
    font-weight: bolder;
  }
  #tags button.urgent label {
    color: @base08;
    font-weight: bolder;
  }

  #window {
    color: @base0E;
    margin-left: 8px;
    margin-right: 8px;
  }

  #mpris {
    color: @base05;
    min-width: 20px;
    /* font-size: 0.9rem; */
    font-weight: lighter;
    padding-left: 16px;
    padding-right: 8px;
  }

  .modules-right .module {
    color: @base05;
    font-size: 1rem;
    font-weight: lighter;
    margin-left: 8px;
    margin-right: 8px;
  }

  #pulseaudio,
  #cpu,
  #memory,
  #disk,
  #battery,
  #power-profiles-daemon,
  #idle_inhibitor {
    font-size: 0.9rem;
  }

  #pulseaudio { color: @base07; }
  #cpu { color: @base08; }
  #memory { color: @base09; }
  #disk { color: @base0A; }
  #battery { color: @base0B; }
  #power-profiles-daemon { color: @base0C; }
  #idle_inhibitor { color: @base0D; }
''
