{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "agent-shell";
  version = "0.55.1";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "v${version}";
    hash = "sha256-tScKFb77tW6ZE7xCjtZ033CfAGzX6AYIM6rNygNggLc=";
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
    homepage = "https://github.com/xenodium/agent-shell";
    description = "An Emacs front-end to shell-based agents";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
