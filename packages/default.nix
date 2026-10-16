{
  pkgs,
  # Passed explicitly by overlays/default.nix (as prev.lib) to avoid forcing
  # final.lib while constructing the overlay's top-level attribute names.
  lib ? pkgs.lib,
  # Whether to include Emacs packages (./emacs/*) in the returned attribute set.
  # Enabled by default for self.packages (so nix-update / update-my-packages can
  # find them), and disabled in overlays/default.nix where Emacs packages are
  # scoped under emacsPackagesFor instead of top-level pkgs.
  includeEmacs ? true,
}:
let
  inherit ((import ../lib/umport.nix { inherit lib; })) umport;

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
        name = lib.strings.removeSuffix ".nix" (baseNameOf f);
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
        name = lib.strings.removeSuffix ".nix" (baseNameOf f);
        inherit value;
      }
    ) emacsFiles
  );

  allOtherPkgs = standardPkgs // emacsPkgs;

  updatableNames = builtins.attrNames (
    lib.filterAttrs (_name: value: value ? updateScript) allOtherPkgs
  );

  update-my-packages = pkgs.callPackage ./nixtools/update-my-packages.nix {
    packageNames = updatableNames;
  };
in
standardPkgs
// lib.optionalAttrs includeEmacs emacsPkgs
// {
  inherit update-my-packages;
}
