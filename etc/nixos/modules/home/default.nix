{ inputs, ... }:

let
  imports = map
    (p: ./. + "/${p}")
    (builtins.filter
      (p: p != "default.nix")
      (builtins.attrNames (builtins.readDir ./.)));
in
{
  imports = [
    inputs.nix-colors.homeManagerModules.default
  ] ++ imports;

  config.nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
}
