{ getopt, nvsrcs, runtimeShell, stdenvNoCC, ... }:

let
  source = nvsrcs.shflags;
in
stdenvNoCC.mkDerivation {
  inherit (source) pname version src;

  buildInputs = [ getopt ];

  doCheck = true;
  checkPhase = ''
    ./test_runner -s ${runtimeShell}
  '';

  installPhase = ''
    runHook preInstall

    install -D -m 755 -t $out/bin shflags

    runHook postInstall
  '';
}
