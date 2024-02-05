{ nvsrcs, stdenvNoCC, p7zip, ... }:

let
  fl = nvsrcs.fsrcnnx-lineart;
  f8 = nvsrcs.fsrcnnx8;
  f16 = nvsrcs.fsrcnnx16;
in
stdenvNoCC.mkDerivation {
  pname = "fsrcnnx";
  inherit (fl) version;

  nativeBuildInputs = [ p7zip ];

  unpackPhase = ''
    7z -aoa x ${fl.src}
  '';

  installPhase = ''
    runHook preInstall

    install -Dm644 FSRCNNX_x2_8-0-4-1_LineArt.glsl $out/FSRCNNX_x2_8-0-4-1_LineArt.glsl
    install -Dm644 ${f8.src} $out/FSRCNNX_x2_8-0-4-1.glsl
    install -Dm644 ${f16.src} $out/FSRCNNX_x2_16-0-4-1.glsl

    runHook postInstall
  '';
}
