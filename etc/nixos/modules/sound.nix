_:

{
  sound.enable = true;

  hardware = {
    # Disable for pipewire
    pulseaudio.enable = false;
  };

  services = {
    # Enable CUPS to print documents.
    printing.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };
  };

  security = {
    # Enabled for pipewire
    rtkit.enable = true;
  };
}
