{
  coreutils,
  fetchgit,
  gnused,
  jq,
  lib,
  melpaBuild,
  nix-prefetch-git,
  writeShellApplication,
}:

melpaBuild {
  pname = "soy-ts-mode";
  version = "0-unstable-2026-06-29";

  src = fetchgit {
    url = "https://p0.pe/my-git-repos/tree-sitter-soy.git";
    rev = "1f38f60f4288f9ce69549ead731bbbf798d9537d";
    hash = "sha256-Kw148XfmhE5+kHpCKxQhhJZpwkmSF1Lb1U175tQQ7q8=";
  };

  recipe = ''
    (soy-ts-mode :fetcher git :url "" :files ("emacs/soy-ts-mode.el"))
  '';

  passthru.updateScript =
    let
      updateScript = writeShellApplication {
        name = "update-soy-ts-mode";
        runtimeInputs = [
          nix-prefetch-git
          jq
          gnused
          coreutils
        ];
        text = ''
          file="packages/emacs/soy-ts-mode.nix"
          url=$(sed -n -E 's/.*url = "([^"]+)";.*/\1/p' "$file" | head -n1)

          metadata=$(nix-prefetch-git "$url")

          rev=$(echo "$metadata" | jq -r '.rev')
          hash=$(echo "$metadata" | jq -r '.hash')
          date=$(echo "$metadata" | jq -r '.date' | cut -d'T' -f1)

          version="0-unstable-$date"

          sed -i -E "s/version = \"[^\"]+\";/version = \"$version\";/" "$file"
          sed -i -E "s/rev = \"[^\"]+\";/rev = \"$rev\";/" "$file"
          sed -i -E "s/hash = \"[^\"]+\";/hash = \"$hash\";/" "$file"

          echo "Updated soy-ts-mode to version $version, rev $rev, hash $hash"
        '';
      };
    in
    [ (lib.getExe updateScript) ];
}
