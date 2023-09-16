{ pkgs, ... }:

{
  home.packages = with pkgs; [
    nil
  ];

  programs = {
    btop = {
      enable = true;
      settings = {
        color_theme = "TTY";
        theme_background = false;
      };
    };
  };
}
