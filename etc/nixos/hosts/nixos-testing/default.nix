# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, inputs, ... }:

{
  imports =
    [
      inputs.hyprland.nixosModules.default
      self.nixosModules.default
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.overlays = [
    inputs.keymapp.overlays.default
    self.overlays.default
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
    xserver.videoDrivers = [ "qxl" ];
  };

  fileSystems."/home/pope/Code/personal" = {
    device = "code_personal";
    fsType = "virtiofs";
  };

  my.nixos = {
    mainUser = "pope";

    bluetooth.enable = true;
    fonts.enable = true;
    onepassword.enable = true;
    sound.enable = true;
    system.enable = true;
    virtualization = {
      enable = true;
      kind = "guest";
    };
    xserver = {
      enable = true;
      desktop = "gnome";
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
