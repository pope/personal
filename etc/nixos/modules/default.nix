{ lib, ... }:

let
  inherit (lib) mkOption types;
in
{
  imports = [
    ./bluetooth.nix
    ./display-manager.nix
    ./firewall-nfs.nix
    ./foldingathome.nix
    ./gaming.nix
    ./gnome.nix
    ./hyprland.nix
    ./nix.nix
    ./onepassword.nix
    ./printing.nix
    ./samba.nix
    ./sound.nix
    ./system.nix
    ./wayland.nix
  ];

  options.my.system = {
    mainUser = mkOption {
      default = "pope";
      description = "The main user of the machine";
      example = "pi";
      type = types.str;
    };
  };

  config = { };
}
