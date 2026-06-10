{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.nixos.yabridge;
in
{
  options.my.nixos.yabridge = {
    enable = lib.mkEnableOption "yabridge options";
  };

  config = lib.mkIf cfg.enable {
    nix-automatic-windows-vsts = {
      enable = true;
      plugins = {
        supermassive = {
          enable = true;
          install = ''
            unzip ${pkgs.valhalla-supermassive.src}
            wine ValhallaSupermassiveWin_V*.exe /SP- /Silent /suppressmsgboxes
          '';
          inputs = [ pkgs.unzip ];
        };
      };
    };
  };
}
