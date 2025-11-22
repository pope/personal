{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.home.editors.emacs;
in
{
  options.my.home.editors.emacs = {
    enable = lib.mkEnableOption "Emacs text editor home options";
    package = lib.mkPackageOption pkgs "emacs-pgtk" { example = "pkgs.emacs-nox"; };
    useSymlink = lib.mkEnableOption "Use a symlink for the init.el file";
    extraConfig = lib.mkOption {
      type = lib.types.lines;
      default = "";
      example = ''
        (setq standard-indent 2)
      '';
      description = ''
        Configuration to include in the Emacs default init file. See
        <https://www.gnu.org/software/emacs/manual/html_node/elisp/Init-File.html>
        for more.

        Note, the `inhibit-startup-message` Emacs option
        cannot be set here since Emacs disallows setting it from the default
        initialization file.
      '';
    };
    extraInit = lib.mkOption {
      type = lib.types.lines;
      default = "";
      example = ''
        (setq standard-indent 2)
      '';
      description = lib.mkDoc ''
        Extra code to add to the end of init.el
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = (cfg.useSymlink && cfg.extraInit == "") || (!cfg.useSymlink);
        message = "Cannot enable the use of a symlink with extraInit info";
      }
    ];
    home = {
      activation.createEmacsDirectories =
        lib.hm.dag.entryAfter [ "writeBoundary" ] # sh
          ''
            run mkdir -m 700 -p $VERBOSE_ARG \
                ${config.home.homeDirectory}/.emacs.d/auto-saves \
                ${config.home.homeDirectory}/.emacs.d/backups
          '';

      file.".emacs.d/init.el" = {
        source = lib.mkIf (cfg.useSymlink || cfg.extraInit == "") (
          if cfg.useSymlink then
            (config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/personal/modules/home/editors/emacs/init.el")
          else
            ./init.el
        );

        text = lib.optionalString (cfg.extraInit != "") ''
          ${builtins.readFile ./init.el}
          ${cfg.extraInit}
        '';
      };

      packages = with pkgs; [
        fd
        ripgrep
        nixd
      ];
    };
    programs.emacs = {
      inherit (cfg) extraConfig;

      enable = true;
      inherit (cfg) package;
      extraPackages =
        epkgs:
        (with epkgs; [
          cape
          catppuccin-theme
          clipetty
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
          expand-region
          fzf
          goto-chg
          indent-bars
          kanagawa-themes
          ligature
          magit
          marginalia
          markdown-mode
          multiple-cursors
          nano-theme
          nerd-icons
          nerd-icons-completion
          nerd-icons-corfu
          nerd-icons-dired
          nerd-icons-ibuffer
          nix-mode
          nix-ts-mode
          notmuch
          nyan-mode
          olivetti
          orderless
          org-modern
          org-superstar
          pink-bliss-uwu-theme
          rg
          sakura-theme
          treesit-grammars.with-all-grammars
          ultra-scroll
          uwu-theme
          vertico
          vterm
          web-mode
          xclip
          zig-mode
          zig-ts-mode
        ]);
    };
  };
}
