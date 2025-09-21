{ self }:

(final: prev:
let
  mypkgs = self.packages.${final.system};
in
{
  inherit (mypkgs) fish-rose-pine fish-catppuccin fish-tokyonight;
  inherit (mypkgs) comic-code-ligatures lucida-grande berkeley-mono;
  inherit (mypkgs) monolisa dank-mono ia-writer;
  inherit (mypkgs) sf-mono-font sf-mono-nf-liga sf-pro;
  inherit (mypkgs) iqm rbutil hatsune-miku-cursor p5r-grub shflags artcnn;
  inherit (mypkgs) add-files-to-nix-store;

  tytools-latest = mypkgs.tytools;

  renoise350 = prev.renoise.override (
    let
      version = "352";
      releasePath =
        if prev.system == "x86_64-linux" then
          (prev.requireFile rec {
            name = "rns_${version}_linux_x86_64.tar.gz";
            url = "file:///media/cyberia/nix-files/software/${name}";
            sha256 = "14ncyi3pzsbl5hy3gdrq62rk6r17mmxl7arnwxapaywz62j6gh2c";
          }) else
          (prev.requireFile rec {
            name = "rns_${version}_linux_arm64.tar.gz";
            url = "file:///media/cyberia/nix-files/software/${name}";
            sha256 = "19jzjvnhic1nndj7xdphl5r79pc3qjbkjrxm0rbl0zjp8s73ici8";
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
