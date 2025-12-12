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
      description = lib.mkDoc ''
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
      (easyaudiosync.overrideAttrs (oldAttrs: {
        patches = oldAttrs.patches ++ [
          ./easyaudiosync.patch
        ];
      }))
      easytag
      fooyin
      lrcget
      puddletag
      tidal-hifi
    ];

    programs.rmpc = {
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
