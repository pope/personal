{ lib, ... }:

let
  inherit (lib) mkOption types;
  imports = map
    (p: ./. + "/${p}")
    (builtins.filter
      (p: p != "default.nix")
      (builtins.attrNames (builtins.readDir ./.)));
in
{
  inherit imports;

  options.my.nixos = {
    mainUser = mkOption {
      default = "pope";
      description = "The main user of the machine";
      example = "pi";
      type = types.str;
    };
  };

  config = { };
}
