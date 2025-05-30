{ config, lib, ... }:

let
  cfg = config.my.nixos.sound;
in
{
  options.my.nixos.sound = {
    enable = lib.mkEnableOption "sound system options";
  };

  config = lib.mkIf cfg.enable {
    services = {
      pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        jack.enable = true;
        pulse.enable = true;
        wireplumber.enable = true;
      };

      # Disable for pipewire
      pulseaudio.enable = false;
    };

    # Enabled for pipewire
    security.rtkit.enable = true;
  };
}
