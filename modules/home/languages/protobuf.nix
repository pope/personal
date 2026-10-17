{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.languages.protobuf;
in
{
  options.my.home.languages.protobuf = {
    enable = lib.mkEnableOption "Protobuf language home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      protobuf
    ];
  };
}
