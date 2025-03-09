{ pkgs, lib, config, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.editors.vscode;
in
{
  options.my.home.editors.vscode = {
    enable = mkEnableOption "VSCode home options";
  };

  config = mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      package = pkgs.vscode.fhsWithPackages (ps: with ps; [ zlib ]);
      mutableExtensionsDir = false;
      profiles.default.extensions = with pkgs.vscode-extensions; [
        bbenoist.nix
        catppuccin.catppuccin-vsc
        catppuccin.catppuccin-vsc-icons
        editorconfig.editorconfig
        enkia.tokyo-night
        golang.go
        mkhl.direnv
        pkgs.stable.vscode-extensions.ms-python.python
        ms-vscode.cmake-tools
        ms-vscode.cpptools
        ms-vscode.cpptools-extension-pack
        ms-vscode.hexeditor
        ms-vscode.makefile-tools
        mvllow.rose-pine
        pkief.material-icon-theme
        pkief.material-product-icons
        rust-lang.rust-analyzer
        twxs.cmake
        vscode-icons-team.vscode-icons
        vscodevim.vim
        yzhang.markdown-all-in-one
        zxh404.vscode-proto3
      ];
    };
  };
}
