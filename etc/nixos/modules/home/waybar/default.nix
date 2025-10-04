{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.home.waybar;
in
{
  options.my.home.waybar = {
    enable = lib.mkEnableOption "waybar home options";
    theme = lib.mkOption {
      type = lib.types.enum [
        "bubble"
        "hyprland"
      ];
      default = "hyprland";
      description = lib.mkDoc ''
        Which waybar theme to use.
      '';
    };
    scale = lib.mkOption {
      type = lib.types.number;
      default = 1;
      example = 0.8;
      description = lib.mkDoc ''
        Scaling to apply to the Waybar sizing.
      '';
    };
  };

  config = lib.mkIf cfg.enable {

    home.packages = with pkgs; [
      libcanberra-gtk3
      playerctl
    ];

    programs = {
      waybar =
        let
          bubble_config = import ./bubblebar/config.nix {
            inherit config pkgs lib;
            inherit (cfg) scale;
          };
          bubble_style = import ./bubblebar/style.nix {
            inherit config lib;
            inherit (cfg) scale;
          };

          hyprland_config = import ./hyprlandbar/config.nix {
            inherit config pkgs;
            inherit (pkgs) hyprland;
          };
          hyprland_style = import ./hyprlandbar/style.nix { inherit config; };
        in
        {
          enable = true;
          settings =
            if cfg.theme == "hyprland" then
              hyprland_config
            else if cfg.theme == "bubble" then
              bubble_config
            else
              abort "unsupported theme";
          style =
            if cfg.theme == "hyprland" then
              hyprland_style
            else if cfg.theme == "bubble" then
              bubble_style
            else
              abort "unsupported theme";
          systemd = {
            enable = true;
            target = "tile-manager-session.target";
          };
        };
    };
  };
}
