{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ctpv
  ];

  programs = {
    lf = {
      enable = true;
      settings = {
        previewer = "${pkgs.ctpv}/bin/ctpv";
        cleaner = "${pkgs.ctpv}/bin/ctpvclear";
      };
    };
  };
}
