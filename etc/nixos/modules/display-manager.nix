_:

{
  services.xserver = {
    enable = true;

    # Enable the GNOME Desktop Environment.
    displayManager = {
      autoLogin = {
        enable = true;
        user = "pope";
      };

      gdm = {
        enable = true;
        # wayland = true;
      };

      # lightdm = {
      #   enable = true;
      # };

      # defaultSession = "gnome";
    };

    # Configure keymap in X11
    layout = "us";
    xkbVariant = "";

    # Enable touchpad support (enabled default in most desktopManager).
    # libinput.enable = true;
  };

  systemd = {
    services = {
      # https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
      "getty@tty1".enable = false;
      "autovt@tty1".enable = false;
    };
  };
}
