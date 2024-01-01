{ pkgs, ... }:

{
  programs.vscode = {
    enable = false;
    package = pkgs.vscodium.fhsWithPackages (ps: with ps; [ zlib ]);
    extensions = with pkgs.vscode-extensions; [
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
}
