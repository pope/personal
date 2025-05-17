{ config, lib, inputs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.editors.neovim;
  inherit (config.my.home.theme) colorScheme;
in
{
  options.my.home.editors.neovim = {
    enable = mkEnableOption "Neovim text editor home options";
  };

  imports = [
    inputs.nixvim.homeManagerModules.nixvim
  ];

  config = mkIf cfg.enable {
    home.sessionVariables.MANPAGER = "nvim +Man!";

    programs.nixvim = _: {
      imports = [
        ../../nixvim
        { config.my.nixvim.theme.colorScheme = colorScheme; }
      ];
      enable = true;
      defaultEditor = true;
    };
  };
}
