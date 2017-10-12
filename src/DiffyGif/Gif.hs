{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module DiffyGif.Gif (
    GifConfig(..)
  , makeGif
  ) where

import Data.Char

import Control.Monad.Except

import Data.Default

import Skylighting

import Codec.Picture.Types
import Codec.Picture.Png
import Codec.Picture.Gif
import Text.Pandoc.Options
import Text.Pandoc.Templates
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.LaTeX
import Data.Hex

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- from image latex render
import Control.Error.Util
import System.IO.Temp
import System.FilePath
import System.Process
import System.Directory
import Control.Monad.Trans.Except
import System.Exit
import Control.Exception

import DiffyGif.Error

data GifConfig =
  GifConfig {
    gcLanguage :: String
  , gcStyle :: Style
  , gcDPI :: Int
  }

instance Default GifConfig where
  def =
    GifConfig "haskell" pygments 200

configBackground ::
  GifConfig ->
  PixelRGB8
configBackground =
  let
    f (RGB r g b) = PixelRGB8 r g b
  in
    maybe (PixelRGB8 245 245 245) f .
    backgroundColor .
    gcStyle

configBackgroundString ::
  GifConfig ->
  String
configBackgroundString c =
  let
    PixelRGB8 r g b = configBackground c
    h = hex . pure . chr . fromIntegral
  in
    mconcat ["#", h r, h g, h b]

palettedMap ::
  (forall pixel. Pixel pixel => Image pixel -> a) ->
  PalettedImage ->
  a
palettedMap f (TrueColorImage d) = dynamicMap f d
palettedMap f (PalettedY8 i _) = f i
palettedMap f (PalettedRGB8 i _) = f i
palettedMap f (PalettedRGBA8 i _) = f i
palettedMap f (PalettedRGB16 i _) = f i

resize ::
  Int ->
  Int ->
  PalettedImage ->
  PalettedImage
resize maxWidth maxHeight ip =
  let
    h i x y =
      if 0 <= x && x < imageWidth i && 0 <= y && y < imageHeight i
      then pixelAt i x y
      else pixelAt i 0 0
  in
    case ip of
      PalettedY8 i p    -> PalettedY8 (generateImage (h i) maxWidth maxHeight) p
      PalettedRGB8 i p  -> PalettedRGB8 (generateImage (h i) maxWidth maxHeight) p
      PalettedRGBA8 i p -> PalettedRGBA8 (generateImage (h i) maxWidth maxHeight) p
      PalettedRGB16 i p -> PalettedRGB16 (generateImage (h i) maxWidth maxHeight) p
      TrueColorImage di -> TrueColorImage di -- should make this explode

codeToMarkdown ::
  GifConfig ->
  String ->
  String
codeToMarkdown c s =
  mconcat [
      "```"
    , gcLanguage c
    , "\n"
    , s
    , "\n```"
    ]

markdownToLatex ::
  GifConfig ->
  String ->
  ExceptT Error IO String
markdownToLatex c s =
  let
     em = readMarkdown def s
  in do
    et <- liftIO $ getDefaultTemplate Nothing "latex"
    t <- case et of
      Left e -> throwError $ EPandocTemplate e
      Right r -> pure r
    case em of
      Left e -> throwError $ EPandoc e
      Right p -> pure $
        writeLaTeX ( def { writerTemplate = Just t
                         , writerHighlight = True
                         , writerHighlightStyle = gcStyle c
                         , writerVariables  = [("header-includes", "\\pagestyle{empty}\n")]
                         } ) p

latexToImage ::
  GifConfig ->
  String ->
  ExceptT Error IO PalettedImage
latexToImage c s = do
  ei <- liftIO $ imageForFormula c defaultEnv s
  case ei of
    Left e -> throwError $ ERender e
    Right x -> pure x

makeGif ::
  GifConfig ->
  [(Int, String)] ->
  ExceptT Error IO BL.ByteString
makeGif c pieces = do
  let
    f s = (markdownToLatex c . codeToMarkdown c $ s) >>= latexToImage c

  images <- traverse (traverse f) pieces

  let
    maxWidth = maximum . fmap (palettedMap imageWidth . snd) $ images
    maxHeight = maximum . fmap (palettedMap imageHeight . snd) $ images
    images' = fmap (fmap $ resize maxWidth maxHeight) images

  let
    g (x, PalettedRGB8 i p) =
      pure (palettedAsImage p, x, i)
    g (x, PalettedRGBA8 i p) =
      pure (dropAlphaLayer (palettedAsImage p), x, i)
    g _ =
      throwError $ EImage "dodgy image"

  bits <- traverse g images'

  case encodeGifImages LoopingForever bits of
    Left e -> throwError $ EImage e
    Right b -> pure b

data TempDirectoryHandling = UseSystemTempDir { nameTemplate :: String }
                           -- ^ A temporary directory with a name based on the given template will be created in the system temporary files location
                           | UseCurrentDir    { nameTemplate :: String }
                           -- ^ A temporary directory with a name based on the given template will be created in the current directory
       deriving (Eq, Show, Read, Ord)

data EnvironmentOptions
     = EnvironmentOptions { latexCommand :: String -- ^ Command to use for @latex@, default is @latex@
                          , dvipsCommand :: String -- ^ Command to use for @dvips@, default is @dvips@
                          , imageMagickCommand :: String -- ^ Command to use for ImageMagick's @convert@, default is @convert@
                          , latexArgs :: [String] -- ^ Any additional arguments for @latex@
                          , dvipsArgs :: [String] -- ^ Any additional arguments for @dvips@
                          , imageMagickArgs :: [String] -- ^ Any additional arguments for @convert@
                          , tempDir :: TempDirectoryHandling -- ^ How to handle temporary files
                          , tempFileBaseName :: String -- ^ The base name to use for the temporary files.
                          }
       deriving (Eq, Show, Read, Ord)

data FormulaOptions
     = FormulaOptions { preamble :: String -- ^ LaTeX preamble to use. Put your @\usepackage@ commands here.@ commands here.
                      , environment :: String -- ^ LaTeX environment in which the equation will be typeset, usually @math@ or @displaymath@
                      , dpi :: Int -- ^ DPI for the image to be rendered at. ~200 is good for retina displays, ~100 works OK for non-retina displays.
                      }
       deriving (Eq, Show, Read, Ord)

-- | Sensible defaults for system environments. Works if @dvips@, @convert@, and @latex@ are recent enough and in your @$PATH@.
defaultEnv :: EnvironmentOptions
defaultEnv = EnvironmentOptions "latex" "dvips" "convert" [] [] [] (UseSystemTempDir "latex-eqn-temp") "working"

-- | A LaTeX formula, e.g @x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}@ for the quadratic formula. Do not include any @$@s to denote the environment, just
--   specify the environment in the 'FormulaOptions' instead.
type Formula = String

-- | Convert a formula into a JuicyPixels 'DynamicImage', also detecting where the typesetting baseline of the image is.
imageForFormula :: GifConfig -> EnvironmentOptions -> Formula -> IO (Either RenderError PalettedImage)
imageForFormula config (EnvironmentOptions {..}) eqn =
    bracket getCurrentDirectory setCurrentDirectory $ const $ withTemp $ \temp -> runExceptT $ do
      let doc = mconcat ["\\nonstopmode\n", eqn]
      io $ writeFile (temp </> tempFileBaseName <.> "tex") doc
      io $ setCurrentDirectory temp
      (c,o,e) <- io $ flip (readProcessWithExitCode latexCommand) "" $ latexArgs ++ [tempFileBaseName <.> "tex"]
      io $ removeFile (tempFileBaseName <.> "tex")
      io $ removeFile (tempFileBaseName <.> "aux")
      when (c /= ExitSuccess) $ do
        io $ removeFile (tempFileBaseName <.> "dvi")
        throwE $ LaTeXFailure (o ++ "\n" ++ e)
      (c',o',e') <- io $ flip (readProcessWithExitCode dvipsCommand) "" $ dvipsArgs ++ ["-q", "-E", "-o", tempFileBaseName <.> "ps", tempFileBaseName <.> "dvi"]
      io $ removeFile (tempFileBaseName <.> "dvi")
      when (c' /= ExitSuccess) $ throwE $ DVIPSFailure (o' ++ "\n" ++ e')
      let bg = configBackgroundString config
      (c'', o'', e'') <- io $ flip (readProcessWithExitCode imageMagickCommand) "" $
                                [ "-density", show (gcDPI config)
                                , "+antialias"
                                , "-background", bg
                                , "-bordercolor", bg
                                , "-border", "3x3"
                                , "-type", "palette"
                                ] ++ imageMagickArgs ++
                                [ tempFileBaseName <.> "ps", tempFileBaseName <.> "png" ]
      io $ removeFile (tempFileBaseName <.> "ps")
      when (c'' /= ExitSuccess) $ throwE $ IMConvertFailure (o'' ++ "\n" ++ e'')
      imgB <- io $ B.readFile (tempFileBaseName <.> "png")
      io $ removeFile $ tempFileBaseName <.> "png"
      case decodePngWithPaletteAndMetadata imgB of
        Left e -> throwError $ ImageReadError e
        Right (i, _) -> pure i
  where
    io = withExceptT IOException . tryIO
    withTemp a = case tempDir of
      UseSystemTempDir f -> withSystemTempDirectory f a
      UseCurrentDir f -> withTempDirectory "." f a
