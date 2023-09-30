_final: prev:
{
  ctpv = (prev.ctpv.override {
    stdenv = prev.llvmPackages_16.stdenv;
  }).overrideAttrs (oldAttrs: {
    meta.platforms = oldAttrs.meta.platforms ++ [ "aarch64-darwin" ];
  });
}
