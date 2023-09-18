_final: prev:
{
  plow = prev.buildGoModule {
    name = "plow";
    src = prev.fetchFromGitHub {
      owner = "six-ddc";
      repo = "plow";
      rev = "343b7510ccfa477d9c0f3d9aeeaa0834e118c44a";
      sha256 = "sha256-TynFq7e4MtZlA5SmGMybhmCVw67yHYgZWffQjuyhTDA=";
    };
    vendorSha256 = "sha256-t2lBPyCn8bu9hLsWmaCGir9egbX0mQR+8kB0RfY7nHE=";
  };
}
