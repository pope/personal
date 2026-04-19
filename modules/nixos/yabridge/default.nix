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
      plugins =
        let
          nvsrcs = pkgs.callPackage ../../../packages/_sources/generated.nix { };
        in
        {
          supermassive = {
            enable = true;
            install = ''
              unzip ${nvsrcs.valhalla-supermassive.src}
              wine ValhallaSupermassiveWin_V*.exe /SP- /Silent /suppressmsgboxes
            '';
            inputs = [ pkgs.unzip ];
          };
        };
    };
  };
}
