{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.languages.rust;
in
{
  options.my.home.languages.rust = {
    enable = lib.mkEnableOption "Rust language home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      cargo
      rust-analyzer
      rustc
      rustfmt
    ];
  };
}
