{
  findutils,
  fzf,
  gnused,
  trashy,
  writeShellApplication,
}:

writeShellApplication {
  name = "trash-helper";
  runtimeInputs = [
    findutils
    fzf
    gnused
    trashy
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
        | sed -E 's|^[^/]*||; s| +$||' \
        | xargs -d '\n' --no-run-if-empty trash "$ACTION" --match=exact --force
    '';
}
