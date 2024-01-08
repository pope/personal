{ pkgs, pkgs-stable, config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf mdDoc;
  cfg = config.my.system.fah;
  myfahclient = pkgs.writeShellScriptBin "my-fah-client" ''
    exec ${pkgs-stable.fahclient}/bin/FAHClient "$@"
  '';
in
{
  options.my.system.fah = {
    enable = mkEnableOption (mdDoc "Folding@Home");
  };

  config = mkIf cfg.enable {
    services.foldingathome = {
      enable = true;
      package = myfahclient;
      user = "ShiftEleven";
      team = 236565;
      extraArgs = [ "--pause-on-start" ];
    };

    environment.systemPackages = with pkgs-stable; [
      fahclient
      fahcontrol
      fahviewer
    ];
  };
}
