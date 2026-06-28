{ pkgs }:
let
  inherit ((import ../lib/umport.nix { inherit (pkgs) lib; })) umport;

  allFiles = umport {
    path = ./.;
    exclude = [
      ./default.nix
      ./emacs/odin-ts-mode.nix
      ./nixtools/update-my-packages.nix
    ];
  };

  odin-ts-mode = pkgs.emacsPackages.callPackage ./emacs/odin-ts-mode.nix { };

  allOtherPkgs =
    builtins.listToAttrs (
      map (
        f:
        let
          value = pkgs.callPackage f { };
        in
        {
          name = pkgs.lib.strings.removeSuffix ".nix" (baseNameOf f);
          inherit value;
        }
      ) allFiles
    )
    // {
      inherit odin-ts-mode;
    };

  updatableNames = builtins.attrNames (
    pkgs.lib.filterAttrs (_name: value: value ? updateScript) allOtherPkgs
  );

  update-my-packages = pkgs.callPackage ./nixtools/update-my-packages.nix {
    packageNames = updatableNames;
  };
in
allOtherPkgs // { inherit update-my-packages; }
