{ self }:

(final: prev:
let
  mypkgs = self.packages.${final.system};
in
{
  inherit (mypkgs) fish-rose-pine fish-catppuccin fsrcnnx modernx plow p5r-grub;
  inherit (mypkgs) comic-code-ligatures lucida-grande;
  inherit (mypkgs) krigBilateral ssimDownscaler ssimSuperRes;
  inherit (mypkgs) iqm rbutil;

  renoise343 = prev.renoise.override (
    let
      version = "343";
      os = if prev.system == "x86_64-linux" then "x86_64" else "arm64";
      basename = "rns_${version}_linux_${os}.tar.gz";
      releasePath = /media/cyberia/nix-files/software/${basename};
    in
    { inherit releasePath; }
  );

} // prev.lib.optionalAttrs prev.stdenv.isDarwin {

  ctpv = (prev.ctpv.override {
    inherit (prev.llvmPackages_16) stdenv;
  }).overrideAttrs (oldAttrs: {
    meta.platforms = oldAttrs.meta.platforms ++ [ "aarch64-darwin" ];
  });

})
