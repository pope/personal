{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.sound;
in
{
  options.my.nixos.sound = {
    enable = mkEnableOption "sound system options";
  };

  config = mkIf cfg.enable {

    sound.enable = true;

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      jack.enable = true;
      pulse.enable = true;
      wireplumber.enable = true;
    };

    # Disable for pipewire
    hardware .pulseaudio.enable = false;

    # Enabled for pipewire
    security.rtkit.enable = true;
  };
}
