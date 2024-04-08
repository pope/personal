{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.dropbox;
in
{
  options.my.home.dropbox = {
    enable = mkEnableOption "Dropbox support home options";
    # TODO(pope): Copy over the config so that the app doesn't need to be
    # set up before the service can run.
    service.enable = mkEnableOption "Enable Dropbox support service";
    gui.enable = mkEnableOption "Enable Dropbox support GUI";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      maestral
    ] ++ lib.optionals cfg.gui.enable (with pkgs; [
      maestral-gui
    ]);

    systemd.user.services = mkIf cfg.service.enable {
      maestral = {
        Unit.Description = "Maestral (Dropbox)";
        Install.WantedBy = [ "default.target" ];

        Service = {
          ExecStart = "${pkgs.maestral}/bin/maestral start --foreground";
          ExecStop = "${pkgs.maestral}/bin/maestral stop";
          Restart = "on-failure";
          Nice = 10;
        };
      };
    };
  };
}
