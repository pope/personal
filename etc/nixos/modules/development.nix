{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gh
    go
    jq
    nodejs
    python3Full
  ];

  programs = {
    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # mtr.enable = true;
    # gnupg.agent = {
    #   enable = true;
    #   enableSSHSupport = true;
    # };

    neovim = {
      enable = true;
      defaultEditor = true;
    };
  };
}

