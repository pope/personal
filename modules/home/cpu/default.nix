{ lib, ... }:

{
  options.my.home.cpu = {
    arch = lib.mkOption {
      type = lib.types.enum [
        "unspecified"
        "znver4"
      ];
      default = "unspecified";
      description = lib.mkDoc ''
        The type of CPU architecture on the machine
      '';
    };
  };
}
