{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    fzf
    gh
    go
    nodejs
    python3Full
    ripgrep
    tldr
    tree
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

