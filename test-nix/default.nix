{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;

  mytexlive = (pkgs.texlive.combine {
    inherit (pkgs.texlive)
    scheme-basic
    framed
    collection-latexrecommended
    collection-fontsrecommended;
  });

  sourceToImage = file: pkgs.stdenv.mkDerivation {
    name = "source-image";
    src = file;
    buildInputs = [mytexlive pkgs.pandoc pkgs.ghostscript pkgs.imagemagick];
    builder = pkgs.writeText "builder.sh" ''
      source $stdenv/setup

      mkdir $out 

      echo "\`\`\`haskell" > out.md
      cat ${file} >> out.md
      echo "\`\`\`" >> out.md

      pandoc out.md --highlight-style pygments --variable "header-includes"="\pagestyle{empty}" --standalone -o out.tex
      latex out.tex
      dvips -q -E -o out.ps out.dvi
      convert -density 200 -background "#f5f5f5" -bordercolor "#f5f5f5" -border 3x3 +antialias -type palette out.ps $out/out.png
    '';
  };

  imagesIn = [ 
    ./test00.hs 
    ./test01.hs 
    ./test02.hs 
    ./test03.hs 
    ./test04.hs
    ];
  
  imagesOut = map sourceToImage imagesIn; 

  imageToCmd = d: "-page +0+0 ${d}/out.png ";

  imagesCmd = nixpkgs.lib.strings.concatMapStrings imageToCmd imagesOut;

  animate = pkgs.stdenv.mkDerivation {
    name = "image-animate";
    src = ./.;
    buildInputs = [pkgs.imagemagick] ++ imagesOut;
    builder = pkgs.writeText "builder.sh" ''
      source $stdenv/setup

      mkdir $out 

      convert -delay 100 -size 800x400 xc:White ${imagesCmd} -loop 0 $out/out.gif
    ''; 
  };
in
  animate
  
