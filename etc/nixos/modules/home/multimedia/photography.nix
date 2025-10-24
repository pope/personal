{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.multimedia.photography;
in
{
  options.my.home.multimedia.photography = {
    enable = lib.mkEnableOption "Photography multimedia home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      darktable
      # TODO(pope): Clean up once https://github.com/NixOS/nixpkgs/pull/453725 is avail
      (digikam.overrideAttrs (_oldAttrs: {
        version = "8.8.0";
        src = fetchFromGitLab {
          domain = "invent.kde.org";
          owner = "graphics";
          repo = "digikam";
          tag = "v8.8.0";
          hash = "sha256-yUrB0FXUcm+6QtlB7HMqdPpdhrV2iAo1oRkjgsHJiCU=";
        };
      }))
      dnglab
      geeqie
      rawtherapee
    ];
  };
}
