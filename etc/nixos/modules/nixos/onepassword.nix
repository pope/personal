{ config, lib, ... }:

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
  };
}
