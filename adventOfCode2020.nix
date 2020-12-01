{ mkDerivation, array, attoparsec, base, bytestring, doctest, lens
, linear, mtl, recursion-schemes, stdenv, text, vector
}:
mkDerivation {
  pname = "adventOfCode2020";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array attoparsec base bytestring doctest lens linear mtl
    recursion-schemes text vector
  ];
  executableHaskellDepends = [ attoparsec base bytestring ];
  testHaskellDepends = [ base ];
  description = "Advent of code 2020";
  license = stdenv.lib.licenses.bsd3;
}
