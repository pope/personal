{ pkgs }:
let
  inherit ((import ../lib/umport.nix { inherit (pkgs) lib; })) umport;

  emacsFiles = umport {
    path = ./emacs;
  };

  standardFiles = umport {
    path = ./.;
    exclude = [
      ./default.nix
      ./nixtools/update-my-packages.nix
    ]
    ++ emacsFiles;
  };

  standardPkgs = builtins.listToAttrs (
    map (
      f:
      let
        value = pkgs.callPackage f { };
      in
      {
        name = pkgs.lib.strings.removeSuffix ".nix" (baseNameOf f);
        inherit value;
      }
    ) standardFiles
  );

  emacsPkgs = builtins.listToAttrs (
    map (
      f:
      let
        value = pkgs.emacsPackages.callPackage f { };
      in
      {
        name = pkgs.lib.strings.removeSuffix ".nix" (baseNameOf f);
        inherit value;
      }
    ) emacsFiles
  );

  allOtherPkgs = standardPkgs // emacsPkgs;

  updatableNames = builtins.attrNames (
    pkgs.lib.filterAttrs (_name: value: value ? updateScript) allOtherPkgs
  );

  update-my-packages = pkgs.callPackage ./nixtools/update-my-packages.nix {
    packageNames = updatableNames;
  };
in
allOtherPkgs // { inherit update-my-packages; }
