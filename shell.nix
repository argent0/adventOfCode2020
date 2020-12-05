{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, attoparsec, base, bytestring
      , containers, doctest, foldl, lens, linear, mtl, recursion-schemes
      , split, stdenv, text, vector
      }:
      mkDerivation {
        pname = "adventOfCode2020";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          array attoparsec base bytestring containers doctest foldl lens
          linear mtl recursion-schemes split text vector
        ];
        executableHaskellDepends = [ attoparsec base bytestring ];
        testHaskellDepends = [ base ];
        description = "Advent of code 2020";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
