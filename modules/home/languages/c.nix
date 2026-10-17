{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.languages.c;
in
{
  options.my.home.languages.c = {
    enable = lib.mkEnableOption "C language home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      with pkgs;
      [
        clang-tools
        cmake
        gnumake
        lldb
        neocmakelsp
        ninja
      ]
      ++ lib.optionals stdenv.hostPlatform.isLinux [
        gcc
        gdb
      ];
  };
}
