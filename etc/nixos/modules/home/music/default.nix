{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.music;
in
{
  options.my.home.music = {
    enable = mkEnableOption "music listening home options";
  };

  config = mkIf cfg.enable {
    programs.rmpc = {
      enable = true;
      config = /* ron */ ''
        #![enable(implicit_some)]
        #![enable(unwrap_newtypes)]
        #![enable(unwrap_variant_newtypes)]
        (
          address: "/run/user/1000/mpd/socket",
          cache_dir: Some("${config.xdg.dataHome}/rmpc"),
        )
      '';
    };

    services = {
      mpd = {
        enable = true;
        extraConfig = ''
          audio_output {
            type "pipewire"
            name "PipeWire Output"
          }
        '';
        network.startWhenNeeded = true;
      };
      mpd-mpris.enable = true;
    };
  };
}
