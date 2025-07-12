{ self }:

(final: prev:
let
  mypkgs = self.packages.${final.system};
in
{
  inherit (mypkgs) fish-rose-pine fish-catppuccin fish-tokyonight;
  inherit (mypkgs) fsrcnnx plow p5r-grub shflags;
  inherit (mypkgs) comic-code-ligatures lucida-grande berkeley-mono;
  inherit (mypkgs) monolisa dank-mono ia-writer;
  inherit (mypkgs) sf-mono-font sf-mono-nf-liga sf-pro;
  inherit (mypkgs) krigBilateral ssimDownscaler ssimSuperRes;
  inherit (mypkgs) iqm rbutil hatsune-miku-cursor;

  tytools-latest = mypkgs.tytools;

  renoise350 = prev.renoise.override (
    let
      version = "350";
      releasePath =
        if prev.system == "x86_64-linux" then
          (prev.requireFile rec {
            name = "rns_${version}_linux_x86_64.tar.gz";
            url = "file:///media/cyberia/nix-files/software/${name}";
            sha256 = "0li11vdg8cm8l3434lwrlmys78fac7ipfgsnd9ng6wmwdppwgqd0";
          }) else
          (prev.requireFile rec {
            name = "rns_${version}_linux_arm64.tar.gz";
            url = "file:///media/cyberia/nix-files/software/${name}";
            sha256 = "06i1nqzyhqrb7zh0d1mmy6wdkgp0sc13xkjy43ksc669k50cnjm5";
          });
    in
    { inherit releasePath; }
  );

  stable = import self.inputs.nixpkgs-stable {
    inherit (prev) system;
    config.allowUnfree = true;
  };
} // prev.lib.optionalAttrs prev.stdenv.isDarwin {

  ctpv = (prev.ctpv.override {
    inherit (prev.llvmPackages_16) stdenv;
  }).overrideAttrs (oldAttrs: {
    meta.platforms = oldAttrs.meta.platforms ++ [ "aarch64-darwin" ];
  });

})
