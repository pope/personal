{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.home.multimedia.music;
in
{
  options.my.home.multimedia.music = {
    enable = lib.mkEnableOption "music listening home options";
    musicDirectory = lib.mkOption {
      type = with lib.types; either path str;
      default = config.xdg.userDirs.music;
      defaultText = "\${config.xdg.userDirs.music}";
      apply = toString; # Prevent copies to Nix store.
      description = ''
        The music directory for songs and lyrics.

        If [](#opt-xdg.userDirs.enable) is
        `true` then the defined XDG music directory is used.
        Otherwise, you must explicitly specify a value.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      an-album-cover
      easyaudiosync
      easytag
      fooyin
      lrcget
      puddletag
      tidal-hifi
    ];

    programs = {
      cava = {
        enable = true;
        settings = {
          color =
            with config.my.home.theme.colors.withHash;
            let
              toStr = color: "\"${color}\"";
            in
            {
              background = "default";
              foreground = "default";
              gradient = 1;
              gradient_count = 6;
              gradient_color_1 = toStr base0B;
              gradient_color_2 = toStr base0C;
              gradient_color_3 = toStr base0D;
              gradient_color_4 = toStr base08;
              gradient_color_5 = toStr base0E;
              gradient_color_6 = toStr base0A;
            };
          general = {
            framerate = 60;
            lower_cutoff_freq = 20;
            higher_cutoff_freq = 20000;
          };
          input = {
            method = "pipewire";
            source = "auto";
          };
          output = {
            method = "noncurses";
            channels = "mono";
            mono_option = "average";
          };
        };
      };

      rmpc = {
        enable = true;
        config = # ron
          ''
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
              lyrics_dir: Some("${cfg.musicDirectory}"),
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
        inherit (cfg) musicDirectory;
        network.startWhenNeeded = true;
      };
      mpd-mpris.enable = true;
    };
  };
}
