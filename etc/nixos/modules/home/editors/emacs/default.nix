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
      activation.createEmacsDirectories = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        run mkdir -m 700 -p $VERBOSE_ARG \
            ${config.home.homeDirectory}/.emacs.d/auto-saves \
            ${config.home.homeDirectory}/.emacs.d/backups
      '';

      file.".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/Code/personal/etc/nixos/modules/home/editors/emacs/init.el";

      packages = with pkgs; [
        nixd
      ];
    };
    programs.emacs = {
      enable = true;
      package = if pkgs.stdenv.isDarwin then pkgs.emacs29 else pkgs.emacs29-pgtk;
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
