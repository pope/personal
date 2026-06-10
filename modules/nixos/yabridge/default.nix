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
            wine ${pkgs.valhalla-supermassive}/ValhallaSupermassive*.exe /SP- /Silent /suppressmsgboxes
          '';
          inputs = [ ];
        };
      };
    };
  };
}
