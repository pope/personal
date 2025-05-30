{ pkgs, config, lib, ... }:

let
  cfg = config.my.nixos.fah;
in
{
  options.my.nixos.fah = {
    enable = lib.mkEnableOption (lib.mdDoc "Folding@Home");
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
