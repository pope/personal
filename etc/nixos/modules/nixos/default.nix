{ lib, ... }:

let
  inherit (lib) mkOption types;
in
{
  imports = [
    ./bluetooth.nix
    ./firewall-nfs.nix
    ./foldingathome.nix
    ./fonts.nix
    ./gaming.nix
    ./nix.nix
    ./onepassword.nix
    ./printing.nix
    ./samba.nix
    ./sound.nix
    ./system.nix
    ./users.nix
    ./wayland.nix
    ./xserver
  ];

  options.my.nixos = {
    mainUser = mkOption {
      default = "pope";
      description = "The main user of the machine";
      example = "pi";
      type = types.str;
    };
  };

  config = { };
}
