{ helpers }:

let
  mkLazyKey =
    { lhs
    , rhs ? null
    , mode ? "n"
    , desc ? null
    }:
    let
      posArgs = builtins.filter (x: x != null) [ lhs rhs ];
    in
    (helpers.listToUnkeyedAttrs posArgs) // { inherit mode desc; };
  mkLazyKeys = bindings: helpers.mkRaw
    (helpers.toLuaObject (builtins.map mkLazyKey bindings));
  lua = x: helpers.mkRaw x;
in
{
  inherit lua mkLazyKey mkLazyKeys;
}
