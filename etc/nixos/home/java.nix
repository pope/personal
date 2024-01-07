{ pkgs, ... }:

{
  home.packages = with pkgs; [
    android-studio
    gradle
  ];

  programs = {
    java.enable = true;
  };
}

