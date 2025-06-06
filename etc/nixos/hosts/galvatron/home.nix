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
      tut
      vale
      (writeShellScriptBin "my-jpeg-archive" ''
        set -o errexit
        set -o nounset

        readonly dir=''${1:-.}
        cd "$dir"

        readonly tmpdir=$(mktemp -d)
        trap "rm -rf $(printf %q "$tmpdir")" EXIT

        ${findutils}/bin/find . -mindepth 1 -maxdepth 1 -iregex '.*\.jpe?g$' \
          | ${parallel}/bin/parallel --no-notice "${jpeg-archive}/bin/jpeg-recompress --quality veryhigh --no-copy {} $(printf %q "$tmpdir")/{}"
        if find "$tmpdir" -mindepth 1 -maxdepth 1 | read
        then
          mv $(printf %q "$tmpdir")/* .
        fi
      '')
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
