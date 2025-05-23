{ inputs, config, pkgs, ... }:

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

  config.nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    package = pkgs.nix;
    registry.nixpkgs.flake = inputs.nixpkgs;
    settings = {
      auto-optimise-store = true;
      builders-use-substitutes = true;
      experimental-features = [ "nix-command" "flakes" ];
      trusted-users = [ config.home.username ];
    };
  };
}
