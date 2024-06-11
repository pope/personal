{ self }:

(final: prev:
let
  mypkgs = self.packages.${final.system};
in
{
  inherit (mypkgs) fish-rose-pine fish-catppuccin fsrcnnx modernx plow p5r-grub;
  inherit (mypkgs) krigBilateral ssimDownscaler ssimSuperRes;

  renoise343 = prev.renoise.override {
    releasePath =
      if prev.system == "x86_64-linux"
      then /media/cyberia/Public/Software/rns_343_linux_x86_64.tar.gz
      else /media/cyberia/Public/Software/rns_343_linux_arm64.tar.gz;
  };

} // prev.lib.optionalAttrs prev.stdenv.isDarwin {

  ctpv = (prev.ctpv.override {
    inherit (prev.llvmPackages_16) stdenv;
  }).overrideAttrs (oldAttrs: {
    meta.platforms = oldAttrs.meta.platforms ++ [ "aarch64-darwin" ];
  });

})
