{ pkgs, ... }:

{
  home.packages = with pkgs; [
    firefox
    nil
  ];
}
