args:

let
  overlays = import ../../overlays args;
in
{
  imports = [
    ../../home/audio.nix
    ../../home/development.nix
    ../../home/git.nix
    ../../home/packages.nix
    ../../home/lf.nix
  ];

  nixpkgs.overlays = with overlays; [
    ctpv
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    git.extraConfig."gpg \"ssh\"".program = "/mnt/c/Users/pope/AppData/Local/1Password/app/8/op-ssh-sign-wsl";
  };
}

