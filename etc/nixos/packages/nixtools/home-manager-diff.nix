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

      GEN_CUR=$(home-manager generations \
        | fzf --border --border-label "Select current generation" \
        | cut -d' ' -f7)
      GEN_PREV=$(home-manager generations \
        | grep -v "$GEN_CUR" \
        | fzf --border --border-label "Select previous generation" \
        | cut -d' ' -f7)

      nvd diff "$GEN_PREV" "$GEN_CUR"
    '';
}
