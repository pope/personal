{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.editor;
in
{
  options.my.home.editor = {
    enable = mkEnableOption "Text editor home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      # TODO(pope): Move to git.nix
      gh
      lua-language-server
      nil
    ];

    programs = {
      neovim = {
        enable = true;
        defaultEditor = true;
      };
    };
  };
}
