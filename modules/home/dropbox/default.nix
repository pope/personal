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
    assertions = [
      {
        assertion = config.my.home.sops.enable;
        message = "sops must be enabled to use dropbox module";
      }
    ];

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

    sops = {
      secrets.maestral-account-id = { };
      templates."maestral.ini" = {
        content =
          lib.generators.toINI
            {
              mkKeyValue = lib.generators.mkKeyValueDefault {
                mkValueString =
                  v:
                  if lib.isList v then
                    lib.strings.concatStrings [
                      "["
                      (lib.strings.concatMapStringsSep ", " (x: "'${lib.escape [ "'" ] x}'") v)
                      "]"
                    ]
                  else if v == true then
                    "True"
                  else if v == false then
                    "False"
                  else
                    lib.generators.mkValueStringDefault { } v;
              } " = ";
            }
            {
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
                excluded_items = [ ];
                max_cpu_percent = 20.0;
                keep_history = 604800;
                upload = true;
                download = true;
              };
              main = {
                version = "20.0";
              };
            };
        path = "${config.xdg.configHome}/maestral/maestral.ini";
        # Needs to be writable since exe does try to save settings
        mode = "0600";
      };
    };

    systemd.user.services = lib.mkIf cfg.service.enable {
      maestral = {
        Unit = {
          Description = "Maestral (Dropbox)";
          # Restarts if any option - other than the account_id - changes.
          X-Restart-Triggers = [
            (builtins.hashString "md5" config.sops.templates."maestral.ini".content)
          ];
        };

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
