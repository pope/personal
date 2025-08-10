{ inputs, pkgs, config, lib, ... }:

let
  cfg = config.my.home.browsers.librewolf;
  ffaddons = inputs.firefox-addons.packages."${pkgs.system}";
in
{
  options.my.home.browsers.librewolf = {
    enable = lib.mkEnableOption "Librewolf browser home options";
  };

  config = lib.mkIf cfg.enable {
    programs.librewolf = {
      enable = true;
      profiles.default = {
        isDefault = true;
        extensions.packages = [
          # TODO(pope): Find a better way to fix this hack.
          # Shout out to https://sourcegraph.com/github.com/niksingh710/ndots@b157c0d7337c37d098f3033343f4ed0822575be9/-/blob/modules/home/browsers/firefox/default.nix?L52-57
          (ffaddons."1password-x-password-manager".overrideAttrs { meta.license = lib.licenses.free; })
        ];
      };
    };
  };
}

