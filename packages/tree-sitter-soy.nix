{
  coreutils,
  fetchgit,
  gnused,
  jq,
  lib,
  nix-prefetch-git,
  tree-sitter,
  writeShellApplication,
}:

tree-sitter.buildGrammar {
  language = "soy";
  version = "0-unstable-2026-08-25";

  src = fetchgit {
    url = "https://p0.pe/my-git-repos/tree-sitter-soy.git";
    rev = "a8a7d39be4e5d1aa92e115d7db7f1b10cd9af9d1";
    hash = "sha256-fQfacG9Tui/E/aq9etrOR+Mb0R5ou6Wya6OMyeyFTVo=";
  };

  passthru.updateScript =
    let
      updateScript = writeShellApplication {
        name = "update-tree-sitter-soy";
        runtimeInputs = [
          nix-prefetch-git
          jq
          gnused
          coreutils
        ];
        text = ''
          file="packages/tree-sitter-soy.nix"
          url=$(sed -n -E 's/.*url = "([^"]+)";.*/\1/p' "$file" | head -n1)

          metadata=$(nix-prefetch-git "$url")

          rev=$(echo "$metadata" | jq -r '.rev')
          hash=$(echo "$metadata" | jq -r '.hash')
          date=$(echo "$metadata" | jq -r '.date' | cut -d'T' -f1)

          version="0-unstable-$date"

          sed -i -E "s/version = \"[^\"]+\";/version = \"$version\";/" "$file"
          sed -i -E "s/rev = \"[^\"]+\";/rev = \"$rev\";/" "$file"
          sed -i -E "s/hash = \"[^\"]+\";/hash = \"$hash\";/" "$file"

          echo "Updated tree-sitter-soy to version $version, rev $rev, hash $hash"
        '';
      };
    in
    [ (lib.getExe updateScript) ];

  meta = {
    homepage = "https://p0.pe/my-git-repos/tree-sitter-soy.git";
    description = "Tree-sitter grammar for Soy";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
