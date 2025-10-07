{
  git,
  writeShellApplication,
}:

writeShellApplication {
  name = "git-code-maintenance";
  runtimeInputs = [
    git
  ];
  text = # sh
    ''
      for repo in "$HOME"/Code/*/.git; do
        pushd "$(dirname "$repo")"
        git maintenance run \
          --task commit-graph \
          --task prefetch \
          --task loose-objects \
          --task incremental-repack \
          --task pack-refs \
          --task gc;
        popd
      done
    '';
}
