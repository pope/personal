{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    android-studio
    gradle
  ];

  programs = {
    java.enable = true;
  };
}

