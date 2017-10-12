{ mkDerivation, base, bytestring, data-default, Diff, directory
, errors, filepath, hex, JuicyPixels, mtl, pandoc, process
, skylighting, stdenv, temporary, transformers
}:
mkDerivation {
  pname = "diffygif";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring data-default Diff directory errors filepath hex
    JuicyPixels mtl pandoc process skylighting temporary transformers
  ];
  executableHaskellDepends = [ base bytestring data-default mtl ];
  license = stdenv.lib.licenses.bsd3;
}
