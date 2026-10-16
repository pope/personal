{ lib, ... }:

{
  options.my.home.cpu = {
    arch = lib.mkOption {
      type = lib.types.enum [
        "unspecified"
        "skylake"
        "znver4"
      ];
      default = "unspecified";
      description = ''
        The type of CPU architecture on the machine
      '';
    };
  };
}
