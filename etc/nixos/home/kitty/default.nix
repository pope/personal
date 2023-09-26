{...}:

{
  programs.kitty = {
    enable = true;
    font = {
      name = "Iosevka";
      size = 12.0;
    };
    settings = {
      adjust_line_height = "125%";
      background_opacity = "0.85";
      clipboard_control = "write-clipboard write-primary read-clipboard-ask read-primary-ask";
    };
    theme = "Rosé Pine Moon";
  };
}
