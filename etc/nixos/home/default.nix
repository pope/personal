{ inputs, ... }:

{
  imports = [
    inputs.anyrun.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    ./anyrun
    ./audio
    ./browsers
    ./dropbox
    ./dunst
    ./editors
    ./git
    ./gnome
    ./gtk
    ./keymapp
    ./languages
    ./lf
    ./packages
    ./shell
    ./terminals
    ./xdg
  ];
}
