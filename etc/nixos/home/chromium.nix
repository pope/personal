{ pkgs, ... }:

{
  programs.chromium = {
    enable = true;
    dictionaries = [
      pkgs.hunspellDictsChromium.en_US
    ];
    extensions = [
      {
        # ublock origin
        id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
      }
      {
        # 1Password
        id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa";
      }
    ];
  };
}
