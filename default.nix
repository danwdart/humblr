{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    nixpkgs = nixpkgs;
  },
  compiler ? "ghc94"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = compiler: nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      humblr = lib.dontHaddock (self.callCabal2nix "humblr" (gitignore ./.) {});
    };
  };
  shell = (myHaskellPackages compiler).shellFor {
    packages = p: [
      p.humblr
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find -type f | grep -v dist-newstyle); do krank $i; done
      doctest src
      cabal update
    '';
    buildInputs = (haskell-tools compiler).defaultBuildTools;
    withHoogle = false;
  };
in
{
  inherit shell;
  humblr_ghc810 = lib.justStaticExecutables (myHaskellPackages "ghc810").humblr;
  humblr_ghc90 = lib.justStaticExecutables (myHaskellPackages "ghc90").humblr;
  humblr_ghc92 = lib.justStaticExecutables (myHaskellPackages "ghc92").humblr;
  humblr_ghc94 = lib.justStaticExecutables (myHaskellPackages "ghc94").humblr;
}