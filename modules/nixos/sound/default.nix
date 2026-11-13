{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.nixos.sound;
in
{
  options.my.nixos.sound = {
    enable = lib.mkEnableOption "sound system options";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      config.services.pipewire.package.jack
    ];

    services = {
      pipewire = {
        enable = true;
        # Tune here instead of in the overlay so we don't rebuild everything
        # that depends on pipewire.
        package = pkgs.withCpuTuning pkgs.pipewire;
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
        pulse.enable = true;
        wireplumber.enable = true;
      };

      # Disable for pipewire
      pulseaudio.enable = false;

      udev = {
        # Roto-Control
        extraRules = ''
          SUBSYSTEM=="usb", ATTR{idVendor}=="2e8a", ATTR{idProduct}=="0003", GROUP="plugdev", MODE="0660", TAG+="uaccess"
        '';
        packages = with pkgs; [
          bitwig-connect-control-panel
        ];
      };
    };

    # Enabled for pipewire
    security.rtkit.enable = true;
  };
}
