{ inputs, ... }:

let
  imports = map
    (p: ./. + "/${p}")
    (builtins.filter
      (p: p != "default.nix"
        # TODO(pope): Turn these into config-based modules.
        && p != "gaming"
        && p != "hyprland"
        && p != "waybar")
      (builtins.attrNames (builtins.readDir ./.)));
in
{
  imports = [
    inputs.anyrun.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default
  ] ++ imports;
}
