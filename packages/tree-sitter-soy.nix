{
  lib,
  fetchgit,
  nix-update-script,
  tree-sitter,
}:

tree-sitter.buildGrammar {
  language = "soy";
  version = "0-unstable-2026-06-28";

  src = fetchgit {
    url = "https://p0.pe/my-git-repos/tree-sitter-soy.git";
    rev = "7d637ad03371e9c66ddaad47453076da16bf3b25";
    hash = "sha256-gNUTUZSV7TqTdmNqUcwZPefnI+QDYXxb2+XnJHxNHRM=";
  };

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };

  meta = {
    homepage = "https://p0.pe/my-git-repos/tree-sitter-soy.git";
    description = "Tree-sitter grammar for Soy";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
