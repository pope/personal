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
      nix copy --to ssh://skrapnel.lan \
          .#nixosConfigurations.{unicron,rumble,skrapnel,ravage}.config.system.build.toplevel
    '';
}
