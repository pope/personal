{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.editors.emacs;
in
{
  options.my.home.editors.emacs = {
    enable = mkEnableOption "Emacs text editor home options";
  };

  config = mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs29-pgtk;
      extraPackages = epkgs: (with epkgs; [
        magit
        markdown-mode
        markdown-ts-mode
        nix-ts-mode
        treesit-auto
        treesit-grammars.with-all-grammars
        zig-ts-mode
      ]);
    };
  };
}

