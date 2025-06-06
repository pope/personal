{ pkgs, config, lib, ... }:

let
  cfg = config.my.nixos.printing;
in
{
  options.my.nixos.printing = {
    enable = lib.mkEnableOption "printing system options";
  };

  config = lib.mkIf cfg.enable {
    services = {
      # For network discovery.
      avahi = {
        enable = true;
        nssmdns4 = true;
        openFirewall = true;
      };

      # Enable CUPS to print documents.
      printing.enable = true;
    };

    environment.systemPackages = with pkgs.stable; [
      canon-cups-ufr2
      cnijfilter2
    ];

    hardware.printers = {
      ensurePrinters = [
        {
          name = "Canon_PRO-100_series";
          description = "Canon PRO 100 series";
          location = "Home";
          deviceUri = "dnssd://Canon%20PRO-100%20series._ipp._tcp.local/?uuid=00000000-0000-1000-8000-60128B82C710";
          model = "drv:///sample.drv/generic.ppd";
          # ppdOptions = {};
        }
      ];
      ensureDefaultPrinter = "Canon_PRO-100_series";
    };
  };
}
