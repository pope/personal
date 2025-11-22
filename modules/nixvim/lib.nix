{ lib }:

let
  mkLazyKey =
    {
      lhs,
      rhs ? null,
      mode ? "n",
      desc ? null,
    }:
    let
      posArgs = builtins.filter (x: x != null) [
        lhs
        rhs
      ];
    in
    (lib.nixvim.listToUnkeyedAttrs posArgs) // { inherit mode desc; };
  mkLazyKeys = bindings: lib.nixvim.mkRaw (lib.nixvim.toLuaObject (builtins.map mkLazyKey bindings));
in
{
  inherit mkLazyKey mkLazyKeys;
}
