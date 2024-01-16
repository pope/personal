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
      package = pkgs.vscodium.fhsWithPackages (ps: with ps; [ zlib ]);
      extensions = with pkgs.vscode-extensions; [
        bbenoist.nix
        golang.go
        ms-python.python
        ms-vscode.cpptools
        mvllow.rose-pine
        rust-lang.rust-analyzer
        vscode-icons-team.vscode-icons
        vscodevim.vim
        yzhang.markdown-all-in-one
        zxh404.vscode-proto3
      ];
    };
  };
}
