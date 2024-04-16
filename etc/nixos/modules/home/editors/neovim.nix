{ pkgs, config, lib, inputs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.editors.neovim;
in
{
  options.my.home.editors.neovim = {
    enable = mkEnableOption "Neovim text editor home options";
  };

  imports = [
    inputs.nixvim.homeManagerModules.nixvim
  ];

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      lua-language-server
      nil
      wget
    ];

    programs.nixvim =  _: {
      imports = [ ../../nixvim ];
      enable = true;
      defaultEditor = true;
    };
  };
}
