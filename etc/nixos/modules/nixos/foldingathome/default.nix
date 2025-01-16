{ pkgs, config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf mdDoc;
  cfg = config.my.nixos.fah;
in
{
  options.my.nixos.fah = {
    enable = mkEnableOption (mdDoc "Folding@Home");
  };

  config = mkIf cfg.enable {
    services.foldingathome = {
      enable = true;
      user = "ShiftEleven";
      team = 236565;
    };

    environment.systemPackages = with pkgs; [
      fahclient
    ];
  };
}
