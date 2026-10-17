{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.my.home.editors.android-studio;
in
{
  options.my.home.editors.android-studio = {
    enable = lib.mkEnableOption "Android Studio home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      android-studio
    ];
  };
}
