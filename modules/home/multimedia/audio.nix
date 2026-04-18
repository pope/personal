{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.multimedia.audio;
in
{
  options.my.home.multimedia.audio = {
    enable = lib.mkEnableOption "Audio tool home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      with pkgs;
      [
        convert-48khz
        convert-to-opus
      ]
      ++ lib.optionals stdenv.isLinux [
        amigo-sampler
        bitwig-studio
        chow-tape-model
        distrho-ports
        lsp-plugins
        qpwgraph
        reaper
        renoise-mine
        stable.surge
        tytools
        vcv-rack-pro
        vital
        zenity # For VCV Rack plugins
      ];

    home.sessionVariables = lib.mkIf pkgs.stdenv.isLinux {
      RACK_SYSTEM_DIR = "${pkgs.vcv-rack-pro}/opt/VCV";
    };
  };
}
