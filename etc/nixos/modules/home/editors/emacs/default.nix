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
    home = {
      file.".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/Code/personal/etc/nixos/modules/home/editors/emacs/init.el";

      packages = with pkgs; [
        nil
      ];
    };
    programs.emacs = {
      enable = true;
      package = pkgs.emacs29-pgtk;
      extraPackages = epkgs: (with epkgs; [
        doom-modeline
        doom-themes
        editorconfig
        evil
        ligature
        magit
        markdown-mode
        nerd-icons
        nix-mode
        treesit-auto
        treesit-grammars.with-all-grammars
        xclip
        zig-mode
      ]);
    };
  };
}
