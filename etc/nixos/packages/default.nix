{ pkgs }:
let
  inherit ((import ../lib/umport.nix { inherit (pkgs) lib; })) umport;
  augmentCallPackage =
    callPackage: defaultArgs: fn: extraArgs:
    let
      f = if builtins.isFunction fn then fn else import fn;
      args = builtins.intersectAttrs (builtins.functionArgs f) defaultArgs;
    in
    callPackage f (args // extraArgs);
  nvsrcs = pkgs.callPackage ./_sources/generated.nix { };
  callPackage = augmentCallPackage pkgs.callPackage { inherit nvsrcs; };
in
builtins.listToAttrs (
  map
    (
      f:
      let
        value = callPackage "${f}" { };
      in
      {
        name = pkgs.lib.strings.removeSuffix ".nix" (builtins.baseNameOf f);
        inherit value;
      }
    )
    (umport {
      path = ./.;
      exclude = [
        ./default.nix
        ./_sources
      ];
    })
)
