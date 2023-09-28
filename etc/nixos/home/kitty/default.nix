_:

{
  programs.kitty = {
    enable = true;
    font = {
      name = "Iosevka";
      size = 11.0;
    };
    settings = {
      adjust_line_height = "125%";
      background_opacity = "0.85";
      clipboard_control = "write-clipboard write-primary read-clipboard-ask read-primary-ask";
    };
    # theme = "Catppuccin-Mocha";
    theme = "Rosé Pine Moon";
  };
}
