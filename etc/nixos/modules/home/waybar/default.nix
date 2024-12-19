{ config, pkgs, lib, inputs, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.home.waybar;
in
{
  options.my.home.waybar = {
    enable = mkEnableOption "waybar home options";
    theme = mkOption {
      type = types.enum [ "dwl" "hyprland" ];
      default = "hyprland";
      description = lib.mkDoc ''
        Which waybar theme to use.
      '';
    };
    scale = mkOption {
      type = types.number;
      default = 1;
      example = 0.8;
      description = lib.mkDoc ''
        Scaling to apply to the Waybar sizing.
      '';
    };
  };

  config = mkIf cfg.enable {

    home.packages = with pkgs; [
      libcanberra-gtk3
      playerctl
    ];

    programs = {
      waybar =
        let
          dwl_config = import ./dwlbar/config.nix { inherit config pkgs; inherit (cfg) scale; };
          dwl_style = import ./dwlbar/style.nix { inherit config lib; inherit (cfg) scale; };

          hyprland_config = import ./hyprlandbar/config.nix { inherit config pkgs; inherit (pkgs) hyprland; };
          hyprland_style = import ./hyprlandbar/style.nix { inherit config; };
        in
        {
          enable = true;
          package = inputs.waybar.packages.${pkgs.system}.waybar;
          settings =
            if cfg.theme == "hyprland" then hyprland_config
            else if cfg.theme == "dwl" then dwl_config
            else abort "unsupported theme";
          style =
            if cfg.theme == "hyprland" then hyprland_style
            else if cfg.theme == "dwl" then dwl_style
            else abort "unsupported theme";
          systemd = {
            enable = true;
            target = "tile-manager-session.target";
          };
        };
    };
  };
}
