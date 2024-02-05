{ nvsrcs, buildGoModule }:
let
  source = nvsrcs.plow;
in
buildGoModule {
  name = source.pname;
  inherit (source) src version;

  vendorHash = "sha256-t2lBPyCn8bu9hLsWmaCGir9egbX0mQR+8kB0RfY7nHE=";
}
