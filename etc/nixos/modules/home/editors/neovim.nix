{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.editors.neovim;
in
{
  options.my.home.editors.neovim = {
    enable = mkEnableOption "Neovim text editor home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      lua-language-server
      nil
      tree-sitter
      wget
    ];

    programs = {
      neovim = {
        enable = true;
        defaultEditor = true;
      };
    };
    xdg.configFile."nvim".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/Code/personal/etc/nvim";
  };
}
