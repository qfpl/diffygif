{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  drv = 
    import ./. { inherit nixpkgs compiler; };
  drvWithTools = 
    pkgs.haskell.lib.addBuildTools drv 
      [ pkgs.cabal-install
        pkgs.haskellPacakges.hoogle
        pkgs.haskellPacakges.hasktags
        pkgs.haskellPacakges.stylish-haskell
        pkgs.haskellPacakges.hlint
        pkgs.haskellPacakges.hindent
      ];
in
  if pkgs.lib.inNixShell then drvWithTools.env else drv
