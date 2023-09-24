_:

{
  services = {
    xserver = {
      enable = true;

      displayManager = {
        autoLogin = {
          enable = true;
          user = "pope";
        };

        lightdm = {
          enable = true;
        };
      };

      desktopManager = {
        pantheon.enable = true;
      };

      # Configure keymap in X11
      layout = "us";
      xkbVariant = "";

      # Enable touchpad support (enabled default in most desktopManager).
      # libinput.enable = true;
    };
  };

  systemd = {
    services = {
      # https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
      "getty@tty1".enable = false;
      "autovt@tty1".enable = false;
    };
  };
}
