{ inputs, stdenvNoCC, p7zip, ... }:

stdenvNoCC.mkDerivation {
  pname = "fsrcnnx";
  version = "0.0.1";

  nativeBuildInputs = [ p7zip ];

  unpackPhase = ''
    7z -aoa x ${inputs.fsrcnnx_lineart}
  '';

  installPhase = ''
    runHook preInstall

    install -Dm644 FSRCNNX_x2_8-0-4-1_LineArt.glsl $out/FSRCNNX_x2_8-0-4-1_LineArt.glsl
    install -Dm644 ${inputs.fsrcnnx8} $out/FSRCNNX_x2_8-0-4-1.glsl
    install -Dm644 ${inputs.fsrcnnx16} $out/FSRCNNX_x2_16-0-4-1.glsl

    runHook postInstall
  '';
}
