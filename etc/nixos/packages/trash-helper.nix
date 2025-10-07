{
  coreutils,
  findutils,
  fzf,
  gawk,
  trashy,
  util-linux,
  writeShellApplication,
}:

writeShellApplication {
  name = "trash-helper";
  runtimeInputs = [
    coreutils
    findutils
    fzf
    gawk
    trashy
    util-linux
  ];
  text = # sh
    ''
      ACTION=$(printf "empty\nrestore" | fzf \
          --border --border-label="Trashy Helper" \
          --header="Which action do you want to take?" \
          --list-border --list-label="Actions")
      trash list \
        | fzf --multi \
            --border --border-label="Trashy Helper" \
            --header="Select trash to $ACTION" \
            --list-border --list-label="Paths" \
        | awk '{$1=$1;print}' \
        | rev \
        | cut -d ' ' -f1 \
        | rev \
        | xargs trash "$ACTION" --match=exact --force
    '';
}
