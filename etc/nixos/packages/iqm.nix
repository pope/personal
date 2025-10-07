{
  nvsrcs,
  lib,
  stdenv,
}:

let
  source = nvsrcs.iqm;
in
stdenv.mkDerivation {
  inherit (source) pname version src;

  installPhase = ''
    runHook preInstall

    install -Dm 755 iqm -t $out/bin/

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/lsalzman/iqm";
    description = "IQM Developer Kit";
    license = licenses.mit;
    mainProgram = "iqm";
  };
}
