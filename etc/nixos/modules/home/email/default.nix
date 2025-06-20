{ config, pkgs, lib, ... }:

let
  cfg = config.my.home.email;
  admin-user = "admin";
  domain = "shifteleven.com";
in
{
  options.my.home.email = {
    enable = lib.mkEnableOption "Email home options";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.my.home.sops.enable;
        message = "sops must be enabled to use Email module";
      }
    ];

    accounts.email.accounts.shifteleven-admin = {
      primary = true;
      address = "${admin-user}@${domain}";
      realName = "K. Adam Christensen";
      passwordCommand = "${pkgs.coreutils}/bin/cat ${config.sops.secrets.shifteleven-email-password.path}";

      flavor = "gmail.com";
      folders = {
        drafts = "[Gmail]/Drafts";
        sent = "[Gmail]/Sent Mail";
        trash = "[Gmail]/Trash";
      };

      aerc.enable = true;
      meli.enable = true;
      mbsync = {
        enable = true;
        create = "maildir";
      };
      neomutt.enable = true;
      notmuch.enable = true;
      msmtp.enable = true;
    };

    programs = {
      aerc = {
        enable = true;
        extraConfig = {
          general.unsafe-accounts-conf = true;
          ui = {
            this-day-time-format = ''"           15:04"'';
            timestamp-format = "2006-01-02 15:04";
            dirlist-right = "{{if .Unread}}{{humanReadable .Unread}}/{{end}}{{if .Exists}}{{humanReadable .Exists}}{{end}}";
            dirlist-tree = true;
            threading-enabled = true;
            icon-encrypted = "󰯄";
            icon-signed = "";
            icon-unknown = "";
            icon-attachment = "";
            icon-new = "";
            icon-old = "";
            icon-replied = "";
            icon-marked = "󰄳";
            icon-flagged = "";
            icon-deleted = "";
          };
          filters = {
            "text/plain" = "wrap -w 80 | colorize";
            "text/html" = "html";
            "text/calendar" = "calendar";
            "message/delivery-status" = "colorize";
            "message/rfc822" = "colorize";
            "image/*" = "${lib.getExe pkgs.catimg} -";
          };
        };
      };
      mbsync.enable = true;
      meli.enable = true;
      msmtp.enable = true;
      neomutt = {
        enable = true;
        sidebar = {
          enable = true;
        };
        sort = "reverse-last-date-received";
        vimKeys = true;
        extraConfig = ''
          set pager_index_lines=10
        '';
      };
    };

    sops.secrets.shifteleven-email-password = { };
  };
}
