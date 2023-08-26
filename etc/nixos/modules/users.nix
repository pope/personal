{ config, pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.pope = {
    isNormalUser = true;
    description = "K. Adam Christensen";
    extraGroups = [ "networkmanager" "wheel" "audio" "video" "kvm" "input" "libvirtd" ];
    packages = [];
  };
}
