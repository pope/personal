{ config, pkgs, ... }:

{
  services = {
    udisks2.enable = true;
    dbus.enable = true;
  };

  xdg = {
    # autostart.enable = true;
    portal = {
      enable = true;
      # extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      # extraPortals = [
      #   pkgs.xdg-desktop-portal
      #   pkgs.xdg-desktop-portal-gtk
      # ];
    };
  };

  environment = {
    sessionVariables = {
      # If your cursor becomes invisible
      WLR_NO_HARDWARE_CURSORS = "1";
      # Hint electron apps to use wayland
      NIXOS_OZONE_WL = "1";

      # https://www.reddit.com/r/NixOS/comments/137j18j/need_guide_on_installing_hyprland/
      # CLUTTER_BACKEND = "wayland";
      # GSETTINGS_SCHEMA_DIR = "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}/glib-2.0/schemas";
      # GTK_USE_PORTAL = "1";
      # NIXOS_XDG_OPEN_USE_PORTAL = "1";
      # POLKIT_AUTH_AGENT = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
      # XDG_CURRENT_DESKTOP = "Hyprland";
      # XDG_SESSION_DESKTOP = "Hyprland";

      # WLR_RENDERER = "vulkan";
      # _JAVA_AWT_WM_NONREPARENTING = "1";
      # MOZ_ENABLE_WAYLAND = "1";

      POPE_XDP = "${pkgs.xdg-desktop-portal}";
      POPE_XDP_GTK = "${pkgs.xdg-desktop-portal-gtk}";
      POPE_XDP_HYPR = "${pkgs.xdg-desktop-portal-hyprland}";
    };

    systemPackages = with pkgs; [
      # steam-run
      xdg-utils
      xdg-desktop-portal
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
      # qt6.qtwayland libsForQt5.qt5.qtwayland
      dunst
      gnome.adwaita-icon-theme
      gnome.gnome-themes-extra
      grim
      gsettings-desktop-schemas
      hyprland-protocols
      killall
      libnotify
      networkmanagerapplet
      nvidia-vaapi-driver
      pavucontrol
      polkit_gnome
      rofi-wayland
      rose-pine-gtk-theme
      rose-pine-icon-theme
      slurp
      swww
      udiskie
      wayland
      wl-clipboard
      wlr-randr
      waybar
      (waybar.overrideAttrs (oldAttrs: {
          mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
          postPatch = (oldAttrs.postPatch or "") + ''
            sed -i 's/zext_workspace_handle_v1_activate(workspace_handle_);/const std::string command = "hyprctl dispatch workspace " + name_;\n\tsystem(command.c_str());/g' src/modules/wlr/workspace_manager.cpp
          '';
        })
      )
    ];
  };

  programs = {
    hyprland = {
      enable = true;
      xwayland.enable = true;
      enableNvidiaPatches = true;
    };
  };
}
