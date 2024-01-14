{ pkgs, config, lib, ... }:

let
  inherit (lib) mkOption types;
  inherit (config.my.nixos) mainUser;
  cfg = config.my.nixos.users;
in
{
  options.my.nixos.users = {
    uid = mkOption {
      type = with types; nullOr int;
      default = null;
      description = lib.mdDoc ''
        The account UID. If the UID is null, a free UID is picked on
        activation.
      '';
    };
    initialPassword = mkOption {
      type = with types; nullOr str;
      default = null;
      description = lib.mdDoc ''
        Specifies the initial password for the user, i.e. the
        password assigned if the user does not already exist. If
        {option}`users.mutableUsers` is true, the password
        can be changed subsequently using the
        {command}`passwd` command. Otherwise, it's
        equivalent to setting the {option}`password`
        option. The same caveat applies: the password specified here
        is world-readable in the Nix store, so it should only be
        used for guest accounts or passwords that will be changed
        promptly.

        Note that the {option}`password` option will override this
        option if both are set.
      '';
    };
  };

  config = {
    programs.fish.enable = true;

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users."${mainUser}" = {
      inherit (cfg) uid initialPassword;
      isNormalUser = true;
      description = "K. Adam Christensen";
      extraGroups = [ "networkmanager" "wheel" "audio" "video" "kvm" "input" "libvirtd" "plugdev" ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGg+9LMpvJUBVCndjopRX7Jm6veGyHkf1ZBI/434K2a4"
      ];
      packages = [ ];
      shell = pkgs.fish;
    };

    users.groups.plugdev = { };
  };
}
