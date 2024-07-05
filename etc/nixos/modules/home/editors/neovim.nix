{ pkgs, pkgs-stable, config, lib, inputs, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.home.editors.neovim;
in
{
  options.my.home.editors.neovim = {
    enable = mkEnableOption "Neovim text editor home options";
    colorScheme = mkOption {
      type = types.enum [ "rose-pine" "catppuccin" "dracula" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which color theme to use.
      '';
    };
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

    programs.nixvim = _: {
      _module.args.pkgs-stable = pkgs-stable;
      imports = [
        ../../nixvim
        { config.my.nixvim.theme.colorScheme = cfg.colorScheme; }
      ];
      enable = true;
      defaultEditor = true;
    };
  };
}
