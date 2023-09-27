{ config, inputs, pkgs, ... }:

{
  programs.anyrun = with config.colorScheme.colors; {
    enable = true;

    config = {
      plugins = with inputs.anyrun.packages.${pkgs.system}; [
        applications
        randr
        rink
        shell
        symbols
        translate
      ];

      closeOnClick = true;
      hideIcons = false;
      hidePluginInfo = true;
      ignoreExclusiveZones = false;
      layer = "overlay";
      maxEntries = null;
      showResultsImmediately = false;
      width.fraction = 0.3;
      y.fraction = 0.2;
    };

    extraCss = ''
      #window,
      #entry,
      #plugin,
      #main {
        background: transparent;
        color: #${base05};
        font-family: Iosevka;
        font-size: 1rem;
      }

      #main {
        background-color: #${base00};
        border-radius: 24px;
        padding: 8px;
      }

      #plugin {
        background-color: #${base01};
        border-radius: 8px;
        padding: 4px;
      }

      #match:selected {
        background-color: #${base0D};
        border-radius: 6px;
      }

      #match-desc {
        font-size: .8rem;
      }

      #entry {
        border-radius: 16px;
        border: none;
        font-size: 1.2rem;
        padding: 4px;
      }
    '';
  };
}
