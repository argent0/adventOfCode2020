{ mkDerivation, array, attoparsec, base, bytestring, containers
, doctest, lens, linear, mtl, recursion-schemes, split, stdenv
, text, vector
}:
mkDerivation {
  pname = "adventOfCode2020";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array attoparsec base bytestring containers doctest lens linear mtl
    recursion-schemes split text vector
  ];
  executableHaskellDepends = [ attoparsec base bytestring ];
  testHaskellDepends = [ base ];
  description = "Advent of code 2020";
  license = stdenv.lib.licenses.bsd3;
}
