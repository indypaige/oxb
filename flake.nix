{
  inputs = {
    pkg.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flk.url = "github:numtide/flake-utils";
    qik.url = "github:indigopaige/qik";
  };

  outputs        = { pkg, flk, qik, ... }:
    flk.lib.eachDefaultSystem (system:
      let
        pkgs = import pkg { inherit system; };
        oxb  = qik.lib.${system}.haskell.mk {
          sourceOverrides = {
            org-parser = ./org-mode-hs/org-parser;
            multiwalk  = ./multiwalk;
          };

          name = "oxb-hs";
          root = ./.;
        };
      in {
        packages.default = oxb;
        packages.export = page: pkgs.stdenv.mkDerivation {
          name = "oxb";
          src  = ./.;

          installPhase = ''
          mkdir -p $out
          cp ${page} blog.org
          ${oxb}/bin/oxb
          cp -r blog/* $out
          '';
        };
      });
}
