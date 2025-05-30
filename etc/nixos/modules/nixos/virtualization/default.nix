{ lib, ... }:

{
  imports = [
    ./guest.nix
    ./host.nix
  ];

  options.my.nixos.virtualization = {
    enable = lib.mkEnableOption "virtualization system options";

    kind = lib.mkOption {
      default = "host";
      description = "Which kind of virtualization settings to use";
      example = "guest";
      type = lib.types.enum [ "host" "guest" ];
    };
  };
}

