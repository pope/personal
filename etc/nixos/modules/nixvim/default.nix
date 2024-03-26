{ pkgs, lib, ... }:

let
  autocmd = import ./autocmd.nix;
  colorschemes = import ./colorschemes.nix;
  extras = import ./extras.nix { inherit pkgs; };
  ftplugins = import ./ftplugins.nix;
  mappings = import ./mappings.nix;
  options = import ./options.nix;
  pope = import ./pope.nix;
in
{
  config = lib.mkMerge [
    autocmd.config
    colorschemes.config
    extras.config
    ftplugins.config
    mappings.config
    options.config
    pope.config
  ];
}
