{ inputs, pkgs, lib, config, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.keymapp;
  keymapp = pkgs.buildFHSUserEnv {
    name = "keymapp";
    runScript = pkgs.writeShellScript "keymapp-wrapper.sh" ''
      exec ${inputs.keymapp}
    '';
    targetPkgs = pkgs: with pkgs; [
      gdk-pixbuf
      glib
      gtk3
      libgudev
      libusb1
      systemd
      webkitgtk
    ];
  };
in
{
  options.my.home.keymapp = {
    enable = mkEnableOption "Keymapp home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      keymapp
      (makeDesktopItem {
        name = "keymapp";
        desktopName = "Keymapp";
        genericName = "Keyboard Mapper";
        exec = "${keymapp}/bin/keymapp";
        # TODO(pope): Use a different icon.
        icon = "utilities-terminal";
        type = "Application";
      })
    ];
  };
}
