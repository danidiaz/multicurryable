{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    packages =
         [
            (pkgs.haskellPackages.ghcWithPackages (hpkgs : [
                hpkgs.wai-app-static
                hpkgs.sop-core
                ]))
            pkgs.cabal-install
            pkgs.haskell-language-server
         ];
}
