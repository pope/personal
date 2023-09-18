_:

{
  networking = {
    firewall = {
      allowedTCPPorts = [
        111 #nfs
        2049 #nfs
        20048 # mountd
      ];
      allowedUDPPorts = [
        111 #nfs
        2049 #nfs
        20048 # mountd
      ];
    };
  };
}
