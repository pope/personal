{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.nixos.foldingathome;
in
{
  options.my.nixos.foldingathome = {
    enable = lib.mkEnableOption "Folding@Home";
  };

  config = lib.mkIf cfg.enable {
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
