{ lib, ... }:

let
  inherit (lib) mkEnableOption mkOption types;
in
{
  imports = [
    ./guest.nix
    ./host.nix
  ];

  options.my.nixos.virtualization = {
    enable = mkEnableOption "virtualization system options";

    kind = mkOption {
      default = "host";
      description = "Which kind of virtualization settings to use";
      example = "guest";
      type = types.enum [ "host" "guest" ];
    };
  };
}

