{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.multimedia.photography;
in
{
  options.my.home.multimedia.photography = {
    enable = lib.mkEnableOption "Photography multimedia home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # See https://github.com/NixOS/nixpkgs/pull/426650
      # So defaulting back to the stable version
      (stable.darktable.override {
        # While we are at it, why not try using native optimizations?
        stdenv = impureUseNativeOptimizations stdenv;
      })
      dnglab
      geeqie
      rawtherapee
    ];
  };
}
