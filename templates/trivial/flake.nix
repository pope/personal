{
  description = "TODO";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      nixpkgs,
      self,
      treefmt-nix,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      sys-config =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          treefmt-eval = treefmt-nix.lib.evalModule pkgs (_: {
            projectRootFile = "flake.nix";
            programs = {
              deadnix.enable = true;
              nixfmt.enable = true;
              statix.enable = true;
            };
            settings.global.excludes = [ ".envrc" ];
          });
        in
        {
          devShells.${system}.default = pkgs.mkShell {
            packages = [
              self.formatter.${system}
            ];
          };
          formatter.${system} = treefmt-eval.config.build.wrapper;
          checks.${system}.treefmt = treefmt-eval.config.build.check self;
        };
    in
    builtins.foldl' nixpkgs.lib.recursiveUpdate { } (map sys-config systems);
}
