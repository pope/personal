{
  nix-output-monitor,
  nixos-rebuild,
  writeShellApplication,
}:

writeShellApplication {
  name = "nixos-rebuild-remote";
  runtimeInputs = [
    nix-output-monitor
    nixos-rebuild
  ];
  text = # sh
    ''
      host="$1"
      cmd="''${2:-boot}"
      nixos-rebuild --flake .#"$host" --target-host "$host.lan" --sudo "$cmd" |& nom
    '';
}
