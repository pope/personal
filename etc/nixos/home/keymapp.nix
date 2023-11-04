{ inputs, pkgs, ... }:

{
  home.packages = with pkgs; [
    (pkgs.buildFHSUserEnv {
      name = "keymapp";
      runScript = writeShellScript "keymapp-wrapper.sh" ''
        exec ${inputs.keymapp}
      '';
      # runScript = "/home/pope/Downloads/keymapp";
      targetPkgs = pkgs: with pkgs; [
        gdk-pixbuf
        glib
        gtk3
        libgudev
        libusb1
        systemd
        webkitgtk
      ];
    })
  ];
}
