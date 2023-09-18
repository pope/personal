_:

{
  boot = {
    # I don't believe this is needed if we enable modesetting below.
    # kernelParams = [ "nvidia_drm.modeset=1" ];
    initrd = {
      kernelModules = [ "nvidia_modeset" ];
      availableKernelModules = [ "nvidia_modeset" ];
    };
  };

  hardware = {
    nvidia = {
      # Enabling this feature will enable Wayland; however Steam games get
      # pretty laggy, even though the FPS doesn't drop if not if not run in
      # windowed mode. Even still, Steam itself can be janky and glitchy.
      modesetting.enable = true;
    };
  };

  environment = {
    sessionVariables = {
      # Hint electron apps to use wayland. Otherwise Discord will be janky.
      NIXOS_OZONE_WL = "1";
    };
  };
}
