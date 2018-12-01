{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, stdenv, tasty
      , tasty-discover, tasty-hspec, tasty-th
      }:
      mkDerivation {
        pname = "aoc2018";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base containers ];
        testHaskellDepends = [
          base tasty tasty-discover tasty-hspec tasty-th
        ];
        testToolDepends = [ tasty-discover ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
