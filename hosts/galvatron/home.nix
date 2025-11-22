{ pkgs, ... }:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "pope";
    homeDirectory = "/Users/pope";

    packages = with pkgs; [
      # Fonts
      berkeley-mono
      comic-code-ligatures
      dank-mono
      go-font
      ia-writer
      iosevka
      iosevka-comfy.comfy
      jetbrains-mono
      lucida-grande
      monolisa
      nerd-fonts.gohufont
      nerd-fonts.lilex
      nerd-fonts.symbols-only
      nerd-fonts.terminess-ttf

      clang-tools
      hey
      jpeg-archive
      marksman
      moonlight-qt
      my-jpeg-archive
      obsidian
      tut
      vale
      vscode
    ];

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;

    ssh.matchBlocks."nix-builder" = {
      user = "root";
      hostname = "127.0.0.1";
      port = 3022;
      identityFile = "~/.ssh/insecure_rsa";
    };

    streamlink.enable = true;
  };

  my.home = {
    editors = {
      emacs.enable = true;
      neovim.enable = true;
    };
    git = {
      enable = true;
      opSshSignCommand = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
    };
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    mpv.enable = true;
    multimedia.audio.enable = true;
    packages.enable = true;
    shell.zsh.enable = true;
    ssh = {
      enable = true;
      opIdentityAgent = ''"~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"'';
    };
    terminals = {
      ghostty.enable = true;
      wezterm = {
        enable = true;
        installExtraFonts = true;
      };
    };
    tmux.enable = true;
    yazi.enable = true;
  };
}
