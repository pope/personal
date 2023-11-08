{ inputs, pkgs, ... }:

let
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
}
