{
  nix,
  writeShellApplication,
}:

writeShellApplication {
  name = "nixos-build-to-cache";
  runtimeInputs = [
    nix
  ];
  text = # sh
    ''
      export NIXPKGS_ALLOW_UNFREE=1
      nix copy --to ssh://skrapnel.lan --impure \
          .#nixosConfigurations.{unicron,rumble,skrapnel,ravage}.config.system.build.toplevel
    '';
}
