{ ... }:

{
  networking = {
    firewall = {
      allowedTCPPorts = [
        111 2049 #nfs
        20048 # mountd
      ];
      allowedUDPPorts = [
        111 2049 #nfs
        20048 # mountd
      ];
    };
  };
}
