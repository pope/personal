{ lib, ... }:

let
  inherit (lib) mkOption types;
in
{
  imports = [
    ./bluetooth.nix
    ./display-manager.nix
    ./foldingathome.nix
    ./gaming.nix
    ./gnome.nix
    ./nix.nix
    ./printing.nix
    ./samba.nix
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
