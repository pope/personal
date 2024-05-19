{ config, inputs, pkgs, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.anyrun;
in
{
  options.my.home.anyrun = {
    enable = mkEnableOption "Anyrun home options";
  };

  config = mkIf cfg.enable {
    programs.anyrun = with config.my.home.theme.colors.withHash; {
      enable = true;

      config = {
        plugins = lib.optionals pkgs.stdenv.isLinux [
          inputs.anyrun.packages.${pkgs.system}.applications
          inputs.anyrun.packages.${pkgs.system}.dictionary
          # TODO(pope): Re-enable when the bug is fixed.
          # See https://github.com/anyrun-org/anyrun/issues/153
          # inputs.anyrun.packages.${pkgs.system}.randr
          inputs.anyrun.packages.${pkgs.system}.rink
          inputs.anyrun.packages.${pkgs.system}.stdin
          inputs.anyrun.packages.${pkgs.system}.shell
          inputs.anyrun.packages.${pkgs.system}.symbols
          inputs.anyrun.packages.${pkgs.system}.translate
          inputs.anyrun.packages.${pkgs.system}.websearch
        ];

        closeOnClick = true;
        hideIcons = false;
        hidePluginInfo = false;
        ignoreExclusiveZones = false;
        layer = "overlay";
        maxEntries = null;
        showResultsImmediately = false;
        width.fraction = 0.4;
        y.fraction = 0.2;
      };

      extraConfigFiles = {
        "applications.ron".text = ''
          Config(
            desktop_actions: false,
            max_entries: 7,
            terminal: None,
          )
        '';

        "symbols.ron".text = ''
          Config(
            prefix: "",
            symbols: {
              "kirby": "(>\")>",
              "shrug": "¯\\_(ツ)_/¯",
            },
            max_entries: 4,
          )
        '';
      };

      extraCss = ''
        * {
          color: ${base05};
        }

        #window {
          background: transparent;
        }

        #window > widget > #main {
          background-color: alpha(${base00}, 0.9);
          border-radius: 24px;
          padding: 12px;
        }

        #entry {
          background-color: transparent;
          border-bottom-color: ${base0D};
          border-left-color: ${base0D};
          border-radius: 16px;
          border-right-color: ${base0D};
          border-top-color: ${base0D};
          color: ${base05};
          font-size: 1.2rem;
          padding: 4px;
          padding-left: 8px;
        }

        #main > #main {
          background-color: alpha(${base01}, 0.5);
          border-radius: 16px;
        }

        #main > #main > #plugin {
          background-color: alpha(${base01}, 0.5);
          border-radius: 8px;
          padding: 4px;
        }

        #main > #main > #plugin:hover {
          background-color: alpha(${base02}, 0.5);
        }

        list#plugin {
          background-color: alpha(${base01}, 0.5);
        }

        list#plugin:hover,
        list#plugin #match:hover {
          background-color: alpha(${base02}, 0.5);
        }

        list#plugin #match:active,
        list#plugin #match:focus,
        list#plugin #match:selected {
          background-color: alpha(${base0D}, 0.5);
        }

        list#plugin #match {
          border-radius: 4px;
        }

        #match-desc {
          font-size: .8rem;
        }
      '';
    };
  };
}
