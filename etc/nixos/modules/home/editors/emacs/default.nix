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
      activation.createEmacsDirectories = lib.hm.dag.entryAfter [ "writeBoundary" ] /* sh */ ''
        run mkdir -m 700 -p $VERBOSE_ARG \
            ${config.home.homeDirectory}/.emacs.d/auto-saves \
            ${config.home.homeDirectory}/.emacs.d/backups
      '';

      file.".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/Code/personal/etc/nixos/modules/home/editors/emacs/init.el";

      packages = with pkgs; [
        fd
        ripgrep
        nixd
      ];
    };
    programs.emacs = {
      enable = true;
      package = if pkgs.stdenv.isDarwin then pkgs.emacs30 else pkgs.emacs30-pgtk;
      extraPackages = epkgs: (with epkgs; [
        cape
        consult
        corfu
        corfu-terminal
        diff-hl
        direnv
        doom-modeline
        doom-themes
        editorconfig
        embark
        embark-consult
        evil
        fzf
        indent-bars
        ligature
        magit
        marginalia
        markdown-mode
        nerd-icons
        nerd-icons-completion
        nerd-icons-corfu
        nix-mode
        nix-ts-mode
        nyan-mode
        orderless
        rg
        treesit-grammars.with-all-grammars
        vertico
        vterm
        xclip
        zig-mode
        zig-ts-mode
      ]);
    };
  };
}
