{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    kitty
    kitty-themes
  ];

  programs.kitty = with config.colorScheme.colors; {
    enable = true;
    font = {
      name = "Iosevka";
      package = pkgs.iosevka;
      size = 11.0;
    };
    settings = {
      bold_font = "Iosevka Bold";
      italic_font = "Iosevka Italic";
      bold_italic_font = "Iosevka Bold Italic";

      background_opacity = "0.85";
      clipboard_control = "write-clipboard write-primary read-clipboard-ask read-primary-ask";
      dynamic_background_opacity = "yes";
      shell = "${pkgs.fish}/bin/fish --login";
      tab_bar_edge = "top";
      tab_bar_style = "powerline";
      tab_powerline_style = "round";
      wayland_titlebar_color = "#${base00}";
      window_padding_width = 4;

      "modify_font cell_height" = "125%";
    };
    # theme = "Catppuccin-Mocha";
    theme = "Rosé Pine";
  };
}
