{ pkgs, ... }:

{
  home.packages = with pkgs; [
    cmake
    gcc
    gnumake
    ninja
    stdenv
  ];
}

