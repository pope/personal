{
  rsync,
  writeShellApplication,
}:

writeShellApplication {
  name = "backup-git-repos";
  runtimeInputs = [ rsync ];
  text = # sh
    ''
      rsync -zav --no-links --delete \
          root@shifteleven.com:/home/git/ \
          /media/cyberia/code/git/
    '';
}
