{
  fetchFromGitHub,
  melpaBuild,
  nix-update-script,
}:

melpaBuild {
  pname = "odin-ts-mode";
  version = "0-unstable-2026-06-17";

  src = fetchFromGitHub {
    owner = "Sampie159";
    repo = "odin-ts-mode";
    rev = "138bf6871b5e703ba5ad3f1c3464e2b4ce0fa846";
    hash = "sha256-JaNwVpNhAUmq3mv/44ryvR7hrZywwEqXpRjFqVpfIKo=";
  };

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };
}
