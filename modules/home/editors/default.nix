{ config, lib, ... }:

let
  cfg = config.my.home.editors;
  anyEditorEnabled = lib.any (editor: editor.enable or false) (lib.attrValues cfg);
in
{
  imports = [
    ./android-studio.nix
    ./emacs
    ./neovim.nix
    ./vscode.nix
    ./zed.nix
  ];

  config.assertions = [
    {
      assertion = !anyEditorEnabled || config.my.home.packages.enable;
      message = "my.home.packages.enable must be true when any my.home.editors.* module is enabled.";
    }
  ];
}
