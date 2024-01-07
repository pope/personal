{ pkgs, ... }:

{
  home.packages = with pkgs; [
    maestral
    maestral-gui
  ];
}
