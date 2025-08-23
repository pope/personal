{ config, lib, inputs, ... }:

let
  cfg = config.my.home.editors.neovim;
  inherit (config.my.home.theme) colorScheme;
in
{
  options.my.home.editors.neovim = {
    enable = lib.mkEnableOption "Neovim text editor home options";
  };

  imports = [
    inputs.nixvim.homeModules.nixvim
  ];

  config = lib.mkIf cfg.enable {
    home.sessionVariables.MANPAGER = "nvim +Man!";

    programs.nixvim = _: {
      imports = [
        ../../nixvim
        { config.my.nixvim.theme.colorScheme = colorScheme; }
      ];
      enable = true;
      defaultEditor = true;
      viAlias = true;
      vimAlias = true;
    };
  };
}
