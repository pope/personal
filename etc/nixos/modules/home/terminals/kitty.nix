{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.home.terminals.kitty;
in
{
  options.my.home.terminals.kitty = {
    enable = mkEnableOption "Kitty terminal home options";
    colorScheme = mkOption {
      type = types.enum [ "rose-pine" "catppuccin" "dracula" "tokyonight" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which color theme to use.
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.kitty = with config.my.home.theme.colors.withHash; {
      enable = true;
      font = {
        name = "monospace";
        size = lib.mkDefault 12.0;
      };
      settings = {
        bold_font = "auto";
        italic_font = "auto";
        bold_italic_font = "auto";

        background_opacity = "0.85";
        clipboard_control = "write-clipboard write-primary read-clipboard-ask read-primary-ask";
        dynamic_background_opacity = "yes";
        # shell = "${pkgs.fish}/bin/fish --login";
        tab_bar_edge = "top";
        tab_bar_style = "powerline";
        tab_powerline_style = "round";
        wayland_titlebar_color = base00;
        window_padding_width = 4;

        "modify_font cell_height" = "125%";
      };
      keybindings = {
        #: Increase font size
        "kitty_mod+equal" = "change_font_size all +1.0";
        "kitty_mod+plus" = "change_font_size all +1.0";
        "kitty_mod+kp_add" = "change_font_size all +1.0";
        "cmd+equal" = "change_font_size all +1.0";
        "cmd+plus" = "change_font_size all +1.0";
        "shift+cmd+equal" = "change_font_size all +1.0";

        #: Decrease font size
        "kitty_mod+minus" = "change_font_size all -1.0";
        "kitty_mod+kp_subtract" = "change_font_size all -1.0";
        "cmd+minus" = "change_font_size all -1.0";
        "shift+cmd+minus" = "change_font_size all -1.0";

        #: Reset font size
        "kitty_mod+backspace" = "change_font_size all 0";
        "map cmd+0" = "change_font_size all 0";
      };
      themeFile =
        if cfg.colorScheme == "rose-pine" then "rose-pine"
        else if cfg.colorScheme == "catppuccin" then "Catppuccin-Mocha"
        else if cfg.colorScheme == "dracula" then "Dracula"
        else if cfg.colorScheme == "tokyonight" then "tokyo_night_night"
        else abort "invalid theme";
    };
  };
}
