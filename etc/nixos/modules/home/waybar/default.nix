{ config, pkgs, lib, inputs, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.home.waybar;

  dwl_config = import ./dwlbar/config.nix { inherit config pkgs; };
  dwl_style = import ./dwlbar/style.nix { inherit config; };

  hyprland_config = import ./hyprlandbar/config.nix { inherit config pkgs; inherit (pkgs) hyprland; };
  hyprland_style = import ./hyprlandbar/style.nix { inherit config; };
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
  };

  config = mkIf cfg.enable {

    home.packages = with pkgs; [
      libcanberra-gtk3
    ];

    programs = {
      waybar = {
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
        systemd = mkIf (cfg.theme == "dwl") {
          enable = true;
          target = "dwl-session.target";
        };
      };
    };
  };
}
