((nil . ((eglot-workspace-configuration . (:nixd ( :nixpkgs (:expr "import <nixpkgs> { }")
                                                   :formatting (:command ["nixfmt"])
                                                   :options ( :nixos (:expr "(builtins.getFlake (builtins.toString ./.)).nixosConfigurations.skrapnel.options")
                                                              :home-manager (:expr "(builtins.getFlake (builtins.toString ./.)).homeConfigurations.deck.options"))))))))
