{
  coreutils,
  fzf,
  gnugrep,
  home-manager,
  nvd,
  writeShellApplication,
}:

writeShellApplication {
  name = "home-manager-diff";
  runtimeInputs = [
    coreutils
    fzf
    gnugrep
    home-manager
    nvd
  ];
  text = # sh
    ''
      # A script to select two home-manager generations and find the differences
      # between them.
      FZF_OPTS=(--border --layout=reverse --height 40% --tmux "100%,40%" --margin 2)

      GEN_CUR=$(home-manager generations \
        | fzf "''${FZF_OPTS[@]}" --border-label "Select current generation" \
        | cut -d' ' -f7)
      GEN_PREV=$(home-manager generations \
        | grep -v "$GEN_CUR" \
        | fzf "''${FZF_OPTS[@]}" --border-label "Select previous generation" \
        | cut -d' ' -f7)

      nvd diff "$GEN_PREV" "$GEN_CUR"
    '';
}
