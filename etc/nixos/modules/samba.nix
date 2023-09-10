{ ... }:

{
  networking = {
    firewall = {
      allowedTCPPorts = [
        5357 # wsdd
      ];
      allowedUDPPorts = [
        3702 # wsdd
      ];
    };
  };

  services = {
    avahi = {
      enable = true;
      nssmdns = true;
      publish = {
        enable = true;
        addresses = true;
        domain = true;
        hinfo = true;
        userServices = true;
        workstation = true;
      };
    };

    samba-wsdd.enable = true; # make shares visible for windows 10 clients

    samba = {
      enable = true;
      openFirewall = true;
      securityType = "user";
      extraConfig = ''
        browsable = yes
        smb encrypt = required
        security = user 

        guest account = nobody
        map to guest = bad user
      '';
    };
  };
}
