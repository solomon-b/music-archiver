{
  description = "Archiving/Processing tools for prepping music for my ipod";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    let
      ghcVersion = "982";
      compiler = "ghc${ghcVersion}";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        # need to do the evalPkgs trick so that IFD works with `nix flake check`
        # https://github.com/NixOS/nix/issues/4265
        evalPkgs = import nixpkgs { system = "x86_64-linux"; };

        # Our haskell packages override, needs to use evalPkgs because
        # cabal2nix uses IFD
        hsPkgs = evalPkgs.haskell.packages.${compiler}.override {
          overrides = hfinal: hprev: { music-archiver = hfinal.callCabal2nix "music-archiver" ./. { }; };
        };
      in
      rec {
        overlay = import ./overlay.nix;
        overlays = [
          overlay
        ];

        # Note: cannot reference anything that depends on `evalPkgs` like `hsPkgs`
        # otherwise non-x86_64-linux users will not be able to build the dev env
        devShells = {
          default = hsPkgs.shellFor {
            packages = p: [ p.music-archiver ];
            buildInputs = with pkgs; [
              cabal2nix
              cabal-install
              ghcid
              haskell.compiler.${compiler}
              haskell.packages.${compiler}.haskell-language-server
              just
              pkg-config
              ormolu
              shellcheck
              icu
              zlib
              zlib.dev
            ];
          };
        };

        packages = flake-utils.lib.flattenTree rec { default = hsPkgs.music-archiver; };

        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
