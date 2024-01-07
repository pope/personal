{ pkgs, config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.my.system.fah;
in
{
  options.my.system.fah = {
    enable = mkEnableOption "Folding at Home";
  };

  config = mkIf cfg.enable {

    nixpkgs.config.permittedInsecurePackages = [
      "python-2.7.18.7"
      "python-2.7.18.7-env"
    ];

    services.foldingathome = {
      enable = true;
      user = "ShiftEleven";
      team = 236565;
    };

    environment.systemPackages = with pkgs; [
      # The below doesn't work for me yet. However, I can install and run the
      # controller with:
      # NIXPKGS_ALLOW_UNFREE=1 NIXPKGS_ALLOW_INSECURE=1 nix-shell -p fahcontrol --run FAHControl
      # fahcontrol
      fahviewer
    ];
  };
}
