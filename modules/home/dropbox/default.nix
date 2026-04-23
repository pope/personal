{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.dropbox;
in
{
  options.my.home.dropbox = {
    enable = lib.mkEnableOption "Dropbox support home options";
    service.enable = lib.mkEnableOption "Enable Dropbox support service";
    gui.enable = lib.mkEnableOption "Enable Dropbox support GUI";
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      with pkgs;
      [
        maestral
      ]
      ++ lib.optionals cfg.gui.enable (
        with pkgs;
        [
          maestral-gui
        ]
      );

    # The Config file
    sops.secrets.maestral-account-id = { };
    sops.templates."maestral.ini".content = lib.generators.toINI { } {
      auth = {
        account_id = config.sops.placeholder.maestral-account-id;
        keyring = "keyrings.alt.file.PlaintextKeyring";
        token_access_type = "offline";
      };
      app = {
        notification_level = 15;
        log_level = 20;
        update_notification_interval = 604800;
        bandwidth_limit_up = 0.0;
        bandwidth_limit_down = 0.0;
        max_parallel_uploads = 6;
        max_parallel_downloads = 6;
      };
      sync = {
        path = "/mnt/Cyberia/Dropbox";
        # TODO(pope): Support lists
        excluded_items = "[]";
        max_cpu_percent = 20.0;
        keep_history = 604800;
        upload = true;
        download = true;
      };
      main = {
        version = "20.0";
      };
    };
    xdg.configFile."maestral/maestral.ini".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.templates."maestral.ini".path;

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
