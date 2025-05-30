{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.dropbox;
in
{
  options.my.home.dropbox = {
    enable = lib.mkEnableOption "Dropbox support home options";
    # TODO(pope): Copy over the config so that the app doesn't need to be
    # set up before the service can run.
    service.enable = lib.mkEnableOption "Enable Dropbox support service";
    gui.enable = lib.mkEnableOption "Enable Dropbox support GUI";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      maestral
    ] ++ lib.optionals cfg.gui.enable (with pkgs; [
      maestral-gui
    ]);

    systemd.user.services = lib.mkIf cfg.service.enable {
      maestral = {
        Unit.Description = "Maestral (Dropbox)";
        Install.WantedBy = [ "default.target" ];

        Service = {
          ExecStart = "${lib.getExe pkgs.maestral} start --foreground";
          ExecStop = "${lib.getExe pkgs.maestral} stop";
          Restart = "on-failure";
          Nice = 10;
        };
      };
    };
  };
}
