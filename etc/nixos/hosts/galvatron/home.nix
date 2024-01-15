{ pkgs, ... } @ args:

let
  overlays = import ../../overlays args;
in
{
  imports = [
    ../../home
  ];

  nixpkgs.overlays = with overlays; [
    ctpv
  ];

  home = {
    username = "pope";
    homeDirectory = "/Users/pope";

    packages = with pkgs; [
      clang-tools
      cmake-language-server
      gh
      hey
      jpeg-archive
      lua-language-server
      marksman
      nil
      tut
      vale
    ];

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;

    ssh = {
      controlMaster = "auto";
      controlPath = "/tmp/%r@%h:%p";
      controlPersist = "5m";

      matchBlocks."shifteleven.com".user = "root";

      matchBlocks."nix-builder" = {
        user = "root";
        hostname = "127.0.0.1";
        port = 3022;
        identityFile = "~/.ssh/insecure_rsa";
      };
    };
  };

  my.home = {
    audio.enable = true;
    git = {
      enable = true;
      opIdentityAgent = "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock";
      opSshSignCommand = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
    };
    # TODO(pope) : Enable nvim here.
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    lf.enable = true;
    packages.enable = true;
  };
}
