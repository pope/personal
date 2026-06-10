{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "acp";
  version = "0.12.2";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "acp.el";
    rev = "v${version}";
    hash = "sha256-gtRoM8hdB+opnIPn49KkHwdWoR8qbt9sPdg9TvzQtv8=";
  };

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -d $out/share/emacs/site-lisp
    install *.el $out/share/emacs/site-lisp/
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [ "--flake" ];
  };

  meta = with lib; {
    homepage = "https://github.com/xenodium/acp.el";
    description = "Avatar completion at point (acp) for Emacs";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
