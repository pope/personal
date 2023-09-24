{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cmake
    gcc
    gnumake
    ninja
    stdenv
  ];
}

