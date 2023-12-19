{ pkgs, ... }:

{
  home.packages = with pkgs; [
    cargo
    cmake-language-server
    gh
    go
    gopls
    jq
    lua-language-server
    # nodePackages.pyright
    nodePackages.typescript-language-server
    nodejs
    python311Packages.pyls-isort
    python311Packages.python-lsp-server
    python3Full
    rust-analyzer
    rustc
    rustfmt
    zig
  ];

  programs = {
    neovim = {
      enable = true;
      defaultEditor = true;
    };
  };
}
