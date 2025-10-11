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
      FZF_OPTS=(--border --layout=reverse --height 40% --tmux "100%,40%" --margin 2)

      GEN_CUR=$(find /nix/var/nix/profiles \
          -name "system-*-link" \
          -printf "%CF %CH:%CM : %f -> %l\n" \
        | sort -r \
        | fzf "''${FZF_OPTS[@]}" --border-label "Select current generation" \
        | cut -d' ' -f6)
      GEN_PREV=$(find /nix/var/nix/profiles \
          -name "system-*-link" \
          -printf "%CF %CH:%CM : %f -> %l\n" \
        | sort -r \
        | grep -v "$GEN_CUR" \
        | fzf "''${FZF_OPTS[@]}" --border-label "Select previous generation" \
        | cut -d' ' -f6)

      nvd diff "$GEN_PREV" "$GEN_CUR"
    '';
}
