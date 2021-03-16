{ mkDerivation, array, attoparsec, base, boxes, bytestring
, containers, doctest, foldl, hashable, lens, lib, linear, mtl
, recursion-schemes, split, text, unordered-containers, vector
}:
mkDerivation {
  pname = "adventOfCode2020";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array attoparsec base boxes bytestring containers doctest foldl
    hashable lens linear mtl recursion-schemes split text
    unordered-containers vector
  ];
  executableHaskellDepends = [ attoparsec base bytestring ];
  testHaskellDepends = [ base ];
  description = "Advent of code 2020";
  license = lib.licenses.bsd3;
}
