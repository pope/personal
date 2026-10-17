{
  fetchurl,
  renoise,
  stdenv,
}:

let
  version = "354";
  releasePath =
    if stdenv.hostPlatform.system == "x86_64-linux" then
      fetchurl rec {
        name = "rns_${version}_linux_x86_64.tar.gz";
        url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/software/${name}";
        hash = "sha256-k94/d+syBSOaCy5DWRljc1TzCzRTxPaNlJD4uE5vTFM=";
      }
    else
      fetchurl rec {
        name = "rns_${version}_linux_arm64.tar.gz";
        url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/software/${name}";
        hash = "sha256-ylykdTTW65+SCExuLBhT0gW0BfgqEuhaz3c+krWzbwc=";
      };
in
(renoise.override {
  inherit releasePath;
}).overrideAttrs
  (oldAttrs: {
    passthru = builtins.removeAttrs (oldAttrs.passthru or { }) [ "updateScript" ];
  })
