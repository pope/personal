{
  lib,
  writeShellApplication,
  stdenv,
}:

writeShellApplication {
  name = "add-files-to-nix-store";
  text =
    lib.optionalString stdenv.hostPlatform.isLinux # sh
      ''
        nix-store --add-fixed sha256 \
            /media/cyberia/nix-files/fonts/*.tar.{gz,xz} \
            /media/cyberia/nix-files/software/rns_352_linux_x86_64.tar.gz
      ''
    +
      lib.optionalString stdenv.hostPlatform.isDarwin # sh
        ''
          osascript -e 'mount volume "smb://skrapnel.local/Cyberia"'
          nix-store --add-fixed sha256 \
              /Volumes/Cyberia/nix-files/fonts/*.tar.{gz,xz}
          diskutil unmount /Volumes/Cyberia
        '';
}
