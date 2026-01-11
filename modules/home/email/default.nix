{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.home.email;
  realName = "K. Adam Christensen";

  mkGmailAccount =
    {
      name,
      user,
      domain,
      passwordPath,
      primary ? false,
      patterns ? [ "*" ],
    }:
    {
      inherit primary realName;
      address = "${user}@${domain}";
      passwordCommand = "${pkgs.coreutils}/bin/cat ${passwordPath}";

      flavor = "gmail.com";
      folders = {
        drafts = "[Gmail]/Drafts";
        sent = "[Gmail]/Sent Mail";
        trash = "[Gmail]/Trash";
      };

      aerc = {
        enable = true;
        extraAccounts = {
          source = "notmuch://${config.accounts.email.maildirBasePath}";
          maildir-store = config.accounts.email.maildirBasePath;
          maildir-account-path = name;
        };
      };
      imapnotify = {
        enable = true;
        boxes = [ "INBOX" ];
        onNotify = "${lib.getExe' pkgs.isync "mbsync"} ${name}";
        onNotifyPost = ''
          ${lib.getExe pkgs.mu} index && \
            ${lib.getExe pkgs.notmuch} new && \
            ${lib.getExe' pkgs.libnotify "notify-send"} 'New mail arrived from ${name}'
        '';
      };
      mbsync = {
        enable = true;
        create = "maildir";
        expunge = "both";
        inherit patterns;
      };
      msmtp.enable = true;
      mu.enable = true;
      notmuch.enable = true;
    };
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

    accounts.email.accounts = {
      personal = mkGmailAccount {
        name = "personal";
        user = lib.strings.toLower (builtins.replaceStrings [ "." " " ] [ "" "." ] realName);
        domain = "gmail.com";
        passwordPath = config.sops.secrets.personal-email-password.path;
        primary = true;
        patterns = [
          "INBOX"
          "[Gmail]/Drafts"
          "[Gmail]/Sent Mail"
          "[Gmail]/Trash"
        ];
      };

      shifteleven-admin = mkGmailAccount {
        name = "shifteleven-admin";
        user = "admin";
        domain = "shifteleven.com";
        passwordPath = config.sops.secrets.shifteleven-email-password.path;
      };
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
            "text/plain" = "colorize";
            "text/html" = "html";
            "text/calendar" = "calendar";
            "message/delivery-status" = "colorize";
            "message/rfc822" = "colorize";
            "image/*" = "${lib.getExe pkgs.catimg} -";
          };
        };
      };
      mbsync.enable = true;
      msmtp.enable = true;
      mu.enable = true;
      notmuch.enable = true;
    };

    services.imapnotify.enable = true;

    sops.secrets = {
      personal-email-password = { };
      shifteleven-email-password = { };
    };
  };
}
