{ pkgs, ... }:

{
  # Import all your configuration modules here
  imports = [
    ./autocmd.nix
    ./colorschemes.nix
    ./ftplugins.nix
    ./options.nix
    ./bufferline.nix
    ./pope.nix
    ./mappings.nix
  ];
  extraPlugins = with pkgs; [
    vimPlugins.vim-protobuf
    vimPlugins.vim-startuptime
    (vimUtils.buildVimPlugin {
      name = "treesitter-extra";
      src = ./treesitter-extra;
    })
  ];
}
