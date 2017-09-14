{ mkDerivation, base, bytestring, data-default, Diff, directory
, errors, filepath, JuicyPixels, latex-formulae-image, mtl, pandoc
, process, stdenv, temporary, transformers
}:
mkDerivation {
  pname = "diffygif";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring data-default Diff directory errors filepath
    JuicyPixels latex-formulae-image mtl pandoc process temporary
    transformers
  ];
  executableHaskellDepends = [ base bytestring mtl ];
  license = stdenv.lib.licenses.bsd3;
}
