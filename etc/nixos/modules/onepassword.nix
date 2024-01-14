{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  inherit (config.my.system) mainUser;
  cfg = config.my.system.onepassword;
in
{
  options.my.system.onepassword = {
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
  };
}
