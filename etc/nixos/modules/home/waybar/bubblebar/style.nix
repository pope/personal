{ config, scale, lib }:

let
  color = config.my.home.theme.colors.withHash;

  baseFontSize =
    if lib.isFloat scale then lib.strings.floatToString (1 * scale)
    else builtins.toString (1 * scale);
  smallFontSize = lib.strings.floatToString (0.9 * scale);
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
    font-family: sans-serif, monospace;
    font-size: ${baseFontSize}rem;
    font-weight: normal;
  }

  window#waybar {
    background: alpha(@base00, 0.0);
    color: @base05;
  }

  .modules-left, .modules-right, #mpris {
    background: alpha(@base01, 0.90);
    border: 2px solid @base0E;
    border-radius: 1rem;
    padding-left: 0.25rem;
    padding-right: 0.25rem;
  }
  .modules-left {
    margin-left: 0.5rem;
  }
  .modules-right {
    margin-right: 0.5rem;
  }

  #mpris {
    margin-left: 1rem;
    margin-right: 1rem;
  }

  #custom-nixos {
    margin-left: 0.5rem;
    margin-right: 0.5rem;
  }

  #tags button,
  #workspaces button {
    border: 0px solid black;
    border-radius: 0.5rem;
    box-shadow: none;
    color: @base03;
    min-width: 0px;
    padding: 0.25rem 0.5rem;
  }
  #tags button.focused,
  #workspaces button.active {
    background-color: alpha(@base02, 0.5);
  }
  #tags button:hover,
  #workspaces button:hover {
    background-color: alpha(@base02, 0.5);
    background-image: none;
    border: 0px solid black;
    border-radius: 0.5rem;
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
  #tags button.occupied label,
  #workspaces button label {
    color: @base05;
    font-weight: lighter;
  }
  #tags button.focused label,
  #workspaces button.active  label {
    color: @base0E;
    font-weight: bolder;
  }
  #tags button.urgent label,
  #workspaces button.urgent label {
    color: @base08;
    font-weight: bolder;
  }

  #window {
    color: @base0E;
    font-size: ${smallFontSize}rem;
    margin-left: 0.5rem;
    margin-right: 0.5rem;
  }

  #mpris {
    color: @base05;
    min-width: 20px;
    /* font-size: 0.9rem; */
    padding-left: 1rem;
    padding-right: 0.5rem;
  }

  .modules-right .module {
    color: @base05;
    font-size: ${smallFontSize}rem;
    margin-left: 0.5rem;
    margin-right: 0.5rem;
  }

  #pulseaudio,
  #cpu,
  #memory,
  #disk,
  #battery,
  #power-profiles-daemon,
  #idle_inhibitor {
    font-size: ${smallFontSize}rem;
  }

  #pulseaudio { color: @base07; }
  #cpu { color: @base08; }
  #memory { color: @base09; }
  #disk { color: @base0A; }
  #battery { color: @base0B; }
  #power-profiles-daemon { color: @base0C; }
  #idle_inhibitor { color: @base0D; }
''
