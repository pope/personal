{ pkgs, ... }:

{
  programs.fish.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.pope = {
    isNormalUser = true;
    description = "K. Adam Christensen";
    extraGroups = [ "networkmanager" "wheel" "audio" "video" "kvm" "input" "libvirtd" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGg+9LMpvJUBVCndjopRX7Jm6veGyHkf1ZBI/434K2a4"
    ];
    packages = [ ];
    shell = pkgs.fish;
  };
}
