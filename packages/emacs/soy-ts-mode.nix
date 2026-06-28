{
  fetchgit,
  melpaBuild,
  nix-update-script,
}:

melpaBuild {
  pname = "soy-ts-mode";
  version = "0-unstable-2026-06-28";

  src = fetchgit {
    url = "https://p0.pe/my-git-repos/tree-sitter-soy.git";
    rev = "7d637ad03371e9c66ddaad47453076da16bf3b25";
    hash = "sha256-gNUTUZSV7TqTdmNqUcwZPefnI+QDYXxb2+XnJHxNHRM=";
  };

  recipe = ''
    (soy-ts-mode :fetcher git :url "" :files ("emacs/soy-ts-mode.el"))
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };
}
