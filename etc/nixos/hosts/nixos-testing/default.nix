# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ 
      ../../modules/bluetooth.nix
      ../../modules/gnome.nix
      ../../modules/nix.nix
      ../../modules/sound.nix
      ../../modules/system.nix
      ../../modules/users.nix

      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot = {
    # Bootloader.
    loader = {
      grub = {
        enable = true;
        device = "/dev/vda";
        useOSProber = true;
      };
    };

    # Using this will enable Wayland. I think the modesetting in the nvidia
    # hardware config section will do the same. See that for why this is
    # commented out.
    # kernelParams = [ "nvidia_drm.modeset=1" ];
  };

  networking = {
    hostName = "nixos-testing"; # Define your hostname.
    networkmanager.enable = true;
  };

  # Set your time zone.
  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };

  services = {
    openssh.enable = true;
    qemuGuest.enable = true;
    spice-vdagentd.enable = true;
    xserver.videoDrivers = [ "qxl" ];
  };

  fileSystems."/home/pope/Code/personal" = {
    device = "code_personal";
    fsType = "virtiofs";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
