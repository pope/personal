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
        extraConfig.pipewire.noresample = {
          "context.properties" = {
            "default.clock.allowed-rates" = [
              44100
              48000
              88200
              96000
              192000
            ];
          };
        };
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
