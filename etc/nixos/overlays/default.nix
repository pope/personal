{ self }:

(final: prev:
let
  mypkgs = self.packages.${final.system};
in
{
  inherit (mypkgs) fish-rose-pine fish-catppuccin fish-tokyonight;
  inherit (mypkgs) fsrcnnx modernx plow p5r-grub;
  inherit (mypkgs) comic-code-ligatures lucida-grande monolisa;
  inherit (mypkgs) dank-mono ia-writer sf-mono-font sf-mono-nf-liga;
  inherit (mypkgs) krigBilateral ssimDownscaler ssimSuperRes;
  inherit (mypkgs) iqm rbutil hatsune-miku-cursor yazi-plugins;

  tytools-latest = mypkgs.tytools;

  renoise344 = prev.renoise.override (
    let
      version = "344";
      releasePath =
        if prev.system == "x86_64-linux" then
          (prev.requireFile rec {
            name = "rns_${version}_linux_x86_64.tar.gz";
            url = "file:///media/cyberia/nix-files/software/${name}";
            sha256 = "0qdvqdr5whqmsbiaqw5933p728ckcq3cigzh57bk4hydrrxy22cn";
          }) else
          (prev.requireFile rec {
            name = "rns_${version}_linux_arm64.tar.gz";
            url = "file:///media/cyberia/nix-files/software/${name}";
            sha256 = "0dkmsp5v0rvlppnl1s2hwnfw2ccnhav6rbv1lrck818jprzibjdh";
          });
    in
    { inherit releasePath; }
  );

  owncast = prev.owncast.override {
    buildGoModule = args: final.buildGoModule (args // {
      version = "0.2.3-dev";
      src = final.fetchFromGitHub {
        owner = "owncast";
        repo = "owncast";
        rev = "92584e8306aadfa53c85c8d05473b7279a0aff2a";
        sha256 = "qbIqulRZxQEc1kflnVyUvhtogDEMxcOCWWb6yUlxTCY=";
      };
      vendorHash = "sha256-dwYch4ZFy1B6JC7kJPuV6C9gjxT7giIJNjnSffy9hBM=";
    });
  };

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
