{ nvsrcs
, vimUtils
, ...
}:

let
  source = nvsrcs.neopywal-nvim;
in
vimUtils.buildVimPlugin {
  inherit (source) src version;
  pname = "neopywal.nvim";
  nvimSkipModules = [
    "barbecue.theme.neopywal"
    "neopywal.theme.plugins.airline"
    "neopywal.theme.plugins.barbecue"
    "neopywal.theme.plugins.bufferline"
    "neopywal.theme.plugins.clap"
    "neopywal.theme.plugins.feline"
    "neopywal.theme.plugins.lightline"
    "neopywal.theme.plugins.lualine"
    "neopywal.theme.plugins.reactive"
    "neopywal.utils.kinds"
    "reactive.presets.neopywal-cursor"
    "reactive.presets.neopywal-cursorline"
  ];
}
