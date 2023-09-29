{ pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    font = {
      name = "Iosevka";
      size = 11.0;
    };
    settings = {
      background_opacity = "0.85";
      clipboard_control = "write-clipboard write-primary read-clipboard-ask read-primary-ask";
      dynamic_background_opacity = "yes";
      shell = "${pkgs.fish}/bin/fish --login";
      tab_bar_edge = "top";
      tab_bar_style = "powerline";
      tab_powerline_style = "round";

      "modify_font cell_height" = "125%";
    };
    # theme = "Catppuccin-Mocha";
    theme = "Rosé Pine";
  };
}
