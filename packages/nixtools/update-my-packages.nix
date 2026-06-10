{
  lib,
  nix-update,
  common-updater-scripts,
  writeShellApplication,
  packageNames ? [ ],
}:

writeShellApplication {
  name = "update-my-packages";
  runtimeInputs = [
    nix-update
    common-updater-scripts
  ];
  runtimeEnv = {
    NIXPKGS_ALLOW_UNFREE = 1;
    NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM = 1;
  };
  text = ''
    packages=(${lib.escapeShellArgs packageNames})

    for pkg in "''${packages[@]}"; do
      echo "Updating $pkg..."
      nix-update --flake --use-update-script "$pkg" || true
    done
  '';
}
