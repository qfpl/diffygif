{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  mytexlive = (pkgs.texlive.combine {
    inherit (pkgs.texlive)
    scheme-basic
    collection-latexrecommended
    collection-mathextra;
  });

  haskellPackages = if compiler == "default"
               then pkgs.haskellPackages
               else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./diffgif.nix {};
  drvWithDeps = pkgs.haskell.lib.overrideCabal drv (drv: {
    librarySystemDepends = (drv.librarySystemDepends or []) ++ [mytexlive pkgs.imagemagick pkgs.ghostscript];
  });


in
  drvWithDeps

