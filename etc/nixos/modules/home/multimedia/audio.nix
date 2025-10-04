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
        an-album-cover
        bitwig-studio
        deadbeef-with-plugins
        easyaudiosync
        easytag
        fooyin
        lrcget
        puddletag
        qpwgraph
        reaper
        tytools-latest
        vital
      ];
  };
}
