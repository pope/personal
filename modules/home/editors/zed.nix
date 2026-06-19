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
        nil
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
