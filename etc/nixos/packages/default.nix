{ pkgs }:
let
  inherit (builtins) map filter attrNames readDir listToAttrs;
  augmentCallPackage = callPackage: defaultArgs: fn: extraArgs:
    let
      f = if builtins.isFunction fn then fn else import fn;
      args = builtins.intersectAttrs (builtins.functionArgs f) defaultArgs;
    in
    callPackage f (args // extraArgs);
  nvsrcs = pkgs.callPackage ./_sources/generated.nix { };
  callPackage = augmentCallPackage pkgs.callPackage { inherit nvsrcs; };
in
listToAttrs (map
  (f: {
    name = f;
    value = callPackage (./. + "/${f}") { };
  })
  (filter
    (f: f != "default.nix" && f != "_sources" && f != "nvfetcher.toml")
    (attrNames (readDir ./.))))
