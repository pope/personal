{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.languages.rust;
in
{
  options.my.home.languages.rust = {
    enable = mkEnableOption "Rust language home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cargo
      rust-analyzer
      rustc
      rustfmt
    ];
  };
}

