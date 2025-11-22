{
  findutils,
  fzf,
  fzf-preview,
  kitty,
  swww,
  writeShellApplication,
}:

writeShellApplication {
  name = "pick-background";
  runtimeInputs = [
    findutils
    fzf
    fzf-preview
    kitty
    swww
  ];
  text = # sh
    ''
      if [[ -v GHOSTTY_BIN_DIR ]]; then
        export FZF_PREVIEW_IMAGE_HANDLER=kitty
      else
        export FZF_PREVIEW_IMAGE_HANDLER=sixel
      fi

      find "$HOME/Pictures/backgrounds" -regex '.*\.\(jpg\|png\)$' | \
        fzf --preview='fzf-preview {}' --margin=2 --layout=reverse --border | \
        xargs --no-run-if-empty swww img
    '';
}
