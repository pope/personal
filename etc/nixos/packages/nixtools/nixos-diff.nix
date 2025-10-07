{
  coreutils,
  findutils,
  fzf,
  gnugrep,
  nvd,
  writeShellApplication,
}:

writeShellApplication {
  name = "nixos-diff";
  runtimeInputs = [
    coreutils
    findutils
    fzf
    gnugrep
    nvd
  ];
  text = # sh
    ''
      GEN_CUR=$(find /nix/var/nix/profiles \
          -name "system-*-link" \
          -printf "%CF %CH:%CM : %f -> %l\n" \
        | sort -r \
        | fzf --border --border-label "Select current generation" \
        | cut -d' ' -f6)
      GEN_PREV=$(find /nix/var/nix/profiles \
          -name "system-*-link" \
          -printf "%CF %CH:%CM : %f -> %l\n" \
        | sort -r \
        | grep -v "$GEN_CUR" \
        | fzf --border --border-label "Select previous generation" \
        | cut -d' ' -f6)

      nvd diff "$GEN_PREV" "$GEN_CUR"
    '';
}
