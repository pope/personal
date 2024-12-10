{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  inherit (config.my.nixos) mainUser;
  cfg = config.my.nixos.onepassword;
in
{
  options.my.nixos.onepassword = {
    enable = mkEnableOption "onepassword system options";
  };

  config = mkIf cfg.enable {
    programs = {
      _1password.enable = true;
      _1password-gui = {
        enable = true;
        polkitPolicyOwners = [ mainUser ];
      };
    };

    systemd.user.services.one-password = {
      enable = true;

      description = "1Password";
      script = "${lib.getExe pkgs._1password-gui} --silent";
      partOf = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
    };
  };
}
