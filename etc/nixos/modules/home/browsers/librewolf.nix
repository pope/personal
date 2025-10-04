{ config, lib, ... }:

let
  cfg = config.my.home.browsers.librewolf;
in
{
  options.my.home.browsers.librewolf = {
    enable = lib.mkEnableOption "Librewolf browser home options";
  };

  config = lib.mkIf cfg.enable {
    programs.librewolf = {
      enable = true;
      profiles.default = {
        isDefault = true;
      };
    };
  };
}

