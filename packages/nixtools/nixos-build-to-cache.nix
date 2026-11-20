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
      nix copy \
        --no-check-sigs \
        --option post-build-hook "" \
        --substitute-on-destination \
        --to ssh-ng://skrapnel.lan \
        .#nixosConfigurations.{unicron,rumble,skrapnel,ravage}.config.system.build.toplevel
    '';
}
