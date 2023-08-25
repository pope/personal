# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ 
      ../../modules/system.nix

      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot = {
    # Bootloader.
    loader = {
     systemd-boot.enable = true;
     efi.canTouchEfiVariables = true;
    };

    # Setup keyfile
    initrd.secrets = {
      "/crypto_keyfile.bin" = null;
    };

    # Enable swap on luks
    initrd.luks.devices."luks-4ad17370-d029-41e6-9ef0-cf3fec50df26" = {
      device = "/dev/disk/by-uuid/4ad17370-d029-41e6-9ef0-cf3fec50df26";
      keyFile = "/crypto_keyfile.bin";
    };

    supportedFilesystems = [ "ntfs" ];

    # Using this will enable Wayland. I think the modesetting in the nvidia
    # hardware config section will do the same. See that for why this is
    # commented out.
    # kernelParams = [ "nvidia_drm.modeset=1" ];
  };

  virtualisation.libvirtd.enable = true;

  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    nvidia = {
      # Enabling this feature will enable Wayland; however Steam games get
      # pretty laggy, even though the FPS doesn't drop.
      # Plus, Steam and Discord themselves become pretty janky.
      modesetting.enable = true;
      nvidiaSettings = true;
      open = false;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      powerManagement.enable = true;
    };
  };

  networking = {
    hostName = "soundwave"; # Define your hostname.

    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Configure network proxy if necessary
    # proxy = {
    #   default = "http://user:password@proxy:port/";
    #   noProxy = "127.0.0.1,localhost,internal.domain";
    # };

    # Enable networking
    networkmanager.enable = true;

    # firewall = {
    #   # Open ports in the firewall.
    #   allowedTCPPorts = [ ... ];
    #   allowedUDPPorts = [ ... ];
    #   # Or disable the firewall altogether.
    #   enable = false;
    # };
  };

  # Set your time zone.
  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };

  systemd = {
    services = {
      # https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
      "getty@tty1".enable = false;
      "autovt@tty1".enable = false;
    };
  };

  services = {
    xserver = {
      # Enable the X11 windowing system.
      enable = true;
      videoDrivers = ["nvidia"];

      # Enable the GNOME Desktop Environment.
      displayManager = {
        autoLogin = {
          enable = true;
          user = "pope";
        };

        gdm = {
          enable = true;
          wayland = true;
        };

        # defaultSession = "gnome";
      };

      desktopManager = {
        gnome.enable = true;
      };

      # Configure keymap in X11
      layout = "us";
      xkbVariant = "";

      # Enable touchpad support (enabled default in most desktopManager).
      # libinput.enable = true;
    };

    udisks2.enable = true;

    # Flatpak support
    flatpak.enable = true;
    dbus.enable = true;

    # gvfs.enable = true;
    # gnome = {
    #   sushi.enable = true;
    #   gnome-keyring.enable = true;
    # };

    # Enable the OpenSSH daemon.
    # openssh.enable = true;
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

  security = {
    rtkit.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.pope = {
    isNormalUser = true;
    description = "K. Adam Christensen";
    extraGroups = [ "networkmanager" "wheel" "audio" "video" "kvm" "input" "libvirtd" ];
    packages = with pkgs; [
      firefox
      # thunderbird

      gnomeExtensions.app-icons-taskbar
      gnomeExtensions.appindicator
      gnomeExtensions.blur-my-shell
      gnomeExtensions.caffeine
      gnomeExtensions.forge
      gnomeExtensions.pop-shell
      gnomeExtensions.rounded-window-corners
    ];
  };

  environment.sessionVariables = {
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

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    _1password 
    _1password-gui 
    android-studio
    cmake
    discord
    gradle
    flameshot
    flatpak
    fzf
    gcc
    gh
    git
    gnumake
    go
    goverlay
    gparted
    htop
    kitty
    kitty-themes
    lf
    lutris
    mangohud
    neofetch
    ninja
    nodejs
    nvtop
    obs-studio
    python3Full
    qemu
    ripgrep
    stdenv
    tldr
    tree
    virt-manager
    vkbasalt
    wget

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

  programs = {
    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # mtr.enable = true;
    # gnupg.agent = {
    #   enable = true;
    #   enableSSHSupport = true;
    # };

    java.enable = true;

    steam = {
      enable = true;
      # remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      # dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    };

    neovim = {
      enable = true;
      defaultEditor = true;
    };

    hyprland = {
      enable = true;
      xwayland.enable = true;
      nvidiaPatches = true;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
