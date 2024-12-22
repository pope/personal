{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.flatpak;
in
{
  options.my.nixos.flatpak = {
    enable = mkEnableOption "flatpak options";
  };

  config = mkIf cfg.enable {
    services.flatpak.enable = true;

    environment.systemPackages = with pkgs; [
      flatpak
      (pkgs.writeShellScriptBin "setup-flatpak" ''
        ${pkgs.flatpak}/bin/flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
        ln -s /run/current-system/sw/share/X11/fonts $HOME/.local/share/fonts
        ln -s /run/current-system/sw/share/themes $HOME/.themes
        ${pkgs.flatpak}/bin/flatpak override --user \
            --filesystem=xdg-config/gtk-3.0:ro \
            --filesystem=xdg-config/gtkrc-2.0:ro \
            --filesystem=xdg-config/gtk-4.0:ro \
            --filesystem=xdg-config/gtkrc:ro \
            --filesystem=$HOME/.themes:ro \
            --filesystem=$HOME/.local/share/fonts:ro \
            --filesystem=$HOME/.icons:ro
      '')
    ];
  };
}
