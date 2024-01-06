{ pkgs, ... }:

{
  home.packages = with pkgs; [
    cargo
    cmake-language-server
    gh
    go
    gopls
    lua-language-server
    nil
    # nodePackages.pyright
    nodePackages.typescript-language-server
    nodejs
    python311Packages.pyls-isort
    python311Packages.python-lsp-server
    python3Full
    rust-analyzer
    rustc
    rustfmt
  ];

  programs = {
    neovim = {
      enable = true;
      defaultEditor = true;
    };
  };
}
