{
  fetchFromGitHub,
  melpaBuild,
  nix-update-script,
}:

melpaBuild {
  pname = "odin-ts-mode";
  version = "0-unstable-2026-09-21";

  src = fetchFromGitHub {
    owner = "Sampie159";
    repo = "odin-ts-mode";
    rev = "6a28b1c35f99b879d288056ce0fcaa15d222ba58";
    hash = "sha256-/Zh1Xn5VknOqeKs7HTkMXS8sCq9NjZ56CtfKVWC8nyc=";
  };

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };
}
