{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "shell-maker";
  version = "0.93.1";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "v${version}";
    hash = "sha256-nJNZBZKmzEVn5gNEL0DT4S09Hfd8hk6DLaZTtoRksS0=";
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
    homepage = "https://github.com/xenodium/shell-maker";
    description = "Create Emacs shells using external processes";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
