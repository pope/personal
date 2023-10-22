{ inputs, ... }: (_final: prev: {
  plow = prev.buildGoModule {
    name = "plow";
    version = "1.3.1";
    src = inputs.plow;
    vendorSha256 = "sha256-t2lBPyCn8bu9hLsWmaCGir9egbX0mQR+8kB0RfY7nHE=";
  };
})
