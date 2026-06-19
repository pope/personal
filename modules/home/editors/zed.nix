{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.my.home.editors.zed;
in
{
  options.my.home.editors.zed = {
    enable = lib.mkEnableOption "Zed home options";
  };

  config = lib.mkIf cfg.enable {
    programs.zed-editor = {
      enable = true;
      package = if pkgs.stdenv.isLinux then pkgs.zed-editor-fhs else pkgs.zed-editor;
      extraPackages = with pkgs; [
        cargo
        clang
        clang-tools
        cmake
        cmake-language-server
        gcc
        gdb
        gnumake
        go
        gopls
        jdk
        lldb
        neocmakelsp
        ninja
        nixd
        nodejs
        protobuf
        python3
        rust-analyzer
        rustc
        rustfmt
        typescript-language-server
        zig
        zls
      ];
      mutableUserDebug = true;
      mutableUserKeymaps = true;
      mutableUserSettings = true;
      mutableUserTasks = true;
      userDebug = [ ];
      userKeymaps = [ ];
      userSettings = {
        base_keymap = "VSCode";
        buffer_font_family = "Berkeley Mono";
        buffer_font_size = 15;
        icon_theme = "Zed (Default)";
        languages.Nix.language_servers = [ "nixd" ];
        theme = {
          mode = "dark";
          light = "One Light";
          dark = "One Dark";
        };
        ui_font_size = 16;
      };
      userTasks = [ ];
    };
  };
}
