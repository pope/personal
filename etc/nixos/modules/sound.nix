{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.system.sound;
in
{
  options.my.system.sound = {
    enable = mkEnableOption "sound system options";
  };

  config = mkIf cfg.enable {

    sound.enable = true;

    hardware = {
      # Disable for pipewire
      pulseaudio.enable = false;
    };

    services = {

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
  };
}
