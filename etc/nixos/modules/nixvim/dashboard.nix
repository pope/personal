{ pkgs-stable, ... }:

let
  pkgs = pkgs-stable;
in
{
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = dashboard-nvim;
      dependencies = [ nvim-web-devicons ];
      event = "VimEnter";
      opts.theme = "hyper";
    }
  ];
}
