{ inputs, ... }:

{
  imports = [
    inputs.anyrun.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    ./anyrun.nix
    ./audio.nix
    ./browsers
    ./dunst.nix
    ./editor.nix
    ./git.nix
    ./gnome.nix
    ./gtk
    ./keymapp.nix
    ./languages
    ./lf.nix
    ./packages.nix
    ./shell.nix
    ./terminals
    ./vscode.nix
    ./xdg.nix
  ];
}
