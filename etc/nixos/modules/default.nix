{ config, lib, ... }:

let
  inherit (lib) mkOption types;
in
{
  imports = [
    ./foldingathome.nix
    ./gaming.nix
    ./nix.nix
    ./sound.nix
  ];

  options.my.system = {
    mainUser = mkOption {
      default = "pope";
      description = "The main user of the machine";
      example = "pi";
      type = types.str;
    };
  };

  config = { };
}
