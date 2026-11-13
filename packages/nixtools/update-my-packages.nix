{
  lib,
  nix-update,
  common-updater-scripts,
  writeShellApplication,
  updatablePkgs ? { },
}:

writeShellApplication {
  name = "update-my-packages";
  runtimeInputs = [
    nix-update
    common-updater-scripts
  ];
  text = ''
    failed=()

    ${lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        name: pkg:
        let
          cmd =
            if builtins.isString pkg.updateScript then
              lib.escapeShellArg pkg.updateScript
            else
              "nix-update --flake --use-update-script ${lib.escapeShellArg name}";
        in
        ''
          echo "Updating ${name}..."
          # TODO(pope): Remove after https://github.com/Mic92/nix-update/issues/687 is fixed
          nix flake prefetch >/dev/null
          if ! ${cmd}; then
            failed+=("${name}")
          fi
        ''
      ) updatablePkgs
    )}

    if (( ''${#failed[@]} > 0 )); then
      >&2 echo "Failed to update packages: ''${failed[*]}"
      exit 1
    fi
  '';
}
