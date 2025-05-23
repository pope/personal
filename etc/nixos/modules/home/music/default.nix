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
          artists: (
            album_display_mode: NameOnly,
            album_sort_by: Date,
          ),
          cache_dir: Some("${config.xdg.dataHome}/rmpc"),
          enable_mouse: true,
          lyrics_dir: Some("${config.xdg.userDirs.music}"),
          tabs: [
            (
              name: "Queue",
              pane: Split(
                direction: Horizontal,
                panes: [
                  (size: "60%", pane: Pane(Queue)),
                  (size: "40%", pane: Split(
                    direction: Vertical,
                    panes: [
                      (size: "70%", pane: Pane(AlbumArt)),
                      (size: "30%", pane: Pane(Lyrics)),
                    ],
                  )),
                ],
              ),
            ),
            (
              name: "Directories",
              pane: Pane(Directories),
            ),
            (
              name: "Artists",
              pane: Pane(Artists),
            ),
            (
              name: "Album Artists",
              pane: Pane(AlbumArtists),
            ),
            (
              name: "Albums",
              pane: Pane(Albums),
            ),
            (
              name: "Playlists",
              pane: Pane(Playlists),
            ),
            (
              name: "Search",
              pane: Pane(Search),
            ),
          ],
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
