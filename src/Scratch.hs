{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Scratch where

import Data.Char

import Control.Monad.Except

import Data.Default

import Text.Pandoc.Error
import Text.Pandoc.Options
import Text.Pandoc.Templates
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.LaTeX
import Skylighting
import Data.Hex
import Codec.Picture.Png
import Codec.Picture.Gif
import Codec.Picture.Types
import Data.Algorithm.Diff
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- from image latex render
import Codec.Picture
import Data.Maybe
import Data.List (splitAt)
import Control.Error.Util
import Data.List
import System.IO.Temp
import System.FilePath
import System.Process
import System.Directory
import Control.Monad.Trans.Except
import Control.Monad
import System.Exit
import Control.Exception
import Control.Arrow(second)
import Control.Applicative
import Data.Monoid ((<>))

data Error =
    EPandocTemplate IOException
  | EPandoc PandocError
  | ERender RenderError
  | EImage String
  deriving Show

data Config =
  Config {
    cLanguage :: String
  , cKeyPause :: Int
  , cSectionPause :: Int
  , cStyle :: Style
  }

instance Default Config where
  def = Config "haskell" 10 75 pygments

configBackground :: Config -> PixelRGB8
configBackground =
  let
    f (RGB r g b) = PixelRGB8 r g b
  in
    fromMaybe (PixelRGB8 245 245 245) . 
    fmap f . 
    backgroundColor . 
    cStyle

configBackgroundString :: Config -> String
configBackgroundString c = 
  let
    PixelRGB8 r g b = configBackground c
    h = hex . pure . chr . fromIntegral
  in
    mconcat ["#", h r, h g, h b]

loadAndOrderFiles :: FilePath -> IO [String]
loadAndOrderFiles fp = do
  files <- listDirectory fp
  traverse (readFile . (fp <>)) (sort files)

past :: Diff a -> Maybe a
past (Both x _) = Just x
past (First x)  = Just x
past (Second _) = Nothing

future :: Diff a -> Maybe a
future (Both _ x) = Just x
future (Second x) = Just x
future (First x)  = Nothing

diffs :: Eq a => [a] -> [a] -> [[a]]
diffs s1 s2 =
  let
    diffs = getDiff s1 s2

    fa xs d = case future d of
      Nothing -> xs
      Just x -> x : xs
    a = fmap reverse . scanl fa [] $ diffs

    fb d xs = case past d of
      Nothing -> xs
      Just x -> x : xs
    b = scanr fb [] diffs

    joins = zipWith (++) a b
  in
    map head . group $ joins

interdiffs :: Eq a => [[a]] -> [[[a]]]
interdiffs [] = []
interdiffs xs = zipWith diffs xs (tail xs)

-- we should diff by lines first
-- then diff each line to simulate the typing

-- what about multi-line edits?

-- maybe try to add the newlines first
-- look at each chunked change, and if the number of lines is about to increase, add
-- a frame for the addition of each newline first?

-- this is scoping what we have to lines,
-- but it is not dealing well with line edits
-- (see the Falsish change in test-data-1)
--
-- we possibly want slightly different approaches here for lines and chars
-- lines are added and removed when we have blocks changing at once
--
-- when we have different first and second for lines, we probably
-- want to turn them into strings and switch to the character diff
chunkify :: [String] -> [[String]]
chunkfiy [] =
  []
chunkify xs =
  {-
  let
    ys = fmap lines xs
    res = interdiffs ys
    out = fmap (fmap unlines) res
    out' = concatMap interdiffs out
  in
    out'
   -}
  interdiffs xs

-- possibly generate and use a manifest for the timing
-- - so it can be edited to tweak the whole thing

-- possibly generate nix files to do the image processing
-- - gives us caching, would help while doing editing tweaks
-- - would open the door to stitching together all of the images
--   using ImageMagick


palettedMap :: (forall pixel. Pixel pixel => Image pixel -> a) -> PalettedImage -> a 
palettedMap f (TrueColorImage d) = dynamicMap f d
palettedMap f (PalettedY8 i _) = f i
palettedMap f (PalettedRGB8 i _) = f i
palettedMap f (PalettedRGBA8 i _) = f i
palettedMap f (PalettedRGB16 i _) = f i

makeGif :: Config -> [[String]] -> ExceptT Error IO BL.ByteString
makeGif c pieces = do
  let
    f s = (markdownToLatex c . codeToMarkdown c $ s) >>= latexToImage c

  images <- traverse (traverse f) pieces
  let
    maxWidth = maximum . fmap (palettedMap imageWidth) . mconcat $ images
    maxHeight = maximum . fmap (palettedMap imageHeight) . mconcat $ images

  let
    resize ip =
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
    images' = fmap (fmap resize) images

  let
    g (PalettedRGB8 i p)   = pure (palettedAsImage p, cKeyPause c, i)
    g (PalettedRGBA8 i p)  = pure (dropAlphaLayer (palettedAsImage p), cKeyPause c, i)
    g _ = throwError $ EImage "dodgy image"

  bits <- traverse (traverse g) images'

  let
    modifyPause [] = []
    modifyPause ((p, _, i) : xs) = (p, cSectionPause c, i) : xs
    tweakLast' [] = []
    tweakLast' [(p, _, i)] = [(p, cSectionPause c, i)]
    tweakLast' (x : xs) = x : tweakLast' xs
    tweakLast [] = []
    tweakLast [x] = [tweakLast' x]
    tweakLast (x : xs) = x : tweakLast xs
    bits' = tweakLast . fmap modifyPause $ bits

  case encodeGifImages LoopingForever (mconcat bits') of
    Left e -> throwError $ EImage e
    Right b -> pure b

codeToMarkdown ::
  Config ->
  String ->
  String
codeToMarkdown c s =
  mconcat [
      "```"
    , cLanguage c
    , "\n"
    , s
    , "\n```"
    ]

markdownToLatex :: Config -> String -> ExceptT Error IO String
markdownToLatex c s =
  let
     em = readMarkdown def s
  in do
    et <- liftIO $ getDefaultTemplate Nothing "latex"
    t <- case et of
      Left e -> throwError $ EPandocTemplate e
      Right s -> pure s
    case em of
      Left e -> throwError $ EPandoc e
      Right p -> pure $ 
        writeLaTeX ( def { writerTemplate = Just t
                         , writerHighlight = True
                         , writerHighlightStyle = cStyle c
                         , writerVariables  = [("header-includes", "\\pagestyle{empty}\n")]
                         } ) p

latexToImage :: Config -> String -> ExceptT Error IO PalettedImage
latexToImage c s = do
  ei <- liftIO $ imageForFormula c defaultEnv ( FormulaOptions "" "" 200 ) s
  case ei of
    Left e -> throwError $ ERender e
    Right x -> pure x

-- | This type contains all possible errors than can happen while rendering an equation.
--   It includes all IO errors that can happen as well as more specific errors.
data RenderError = ImageIsEmpty -- ^ The equation produced an empty image
                 | CannotDetectBaseline -- ^ The baseline marker could not be found
                 | LaTeXFailure String -- ^ @latex@ returned a nonzero error code
                 | DVIPSFailure String -- ^ @dvips@ returned a nonzero error code
                 | IMConvertFailure String -- ^ @convert@ returned a nonzero error code
                 | IOException IOException -- ^ An 'IOException' occurred while managing the temporary files used to convert the equation
                 | ImageReadError String -- ^ The PNG image from ImageMagick could not be read by JuicyPixels.
                 deriving (Show, Eq)


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

-- | Use the @amsmath@ package, the @displaymath@ environment, and 200dpi.
displaymath :: FormulaOptions
displaymath = FormulaOptions "\\usepackage{amsmath}" "displaymath" 200

-- | Use the @amsmath@ package, the @math@ environment, and 200dpi.
math :: FormulaOptions
math = FormulaOptions "\\usepackage{amsmath}\\usepackage{amsfonts}\\usepackage{stmaryrd}" "math" 200

-- | Sensible defaults for system environments. Works if @dvips@, @convert@, and @latex@ are recent enough and in your @$PATH@.
defaultEnv :: EnvironmentOptions
defaultEnv = EnvironmentOptions "latex" "dvips" "convert" [] [] [] (UseSystemTempDir "latex-eqn-temp") "working"

-- | A LaTeX formula, e.g @x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}@ for the quadratic formula. Do not include any @$@s to denote the environment, just
--   specify the environment in the 'FormulaOptions' instead.
type Formula = String

-- | Number of pixels from the bottom of the image to the typesetting baseline. Useful for setting your formulae inline with text.
type Baseline = Int

-- | Convert a formula into a JuicyPixels 'DynamicImage', also detecting where the typesetting baseline of the image is.
imageForFormula :: Config -> EnvironmentOptions -> FormulaOptions -> Formula -> IO (Either RenderError PalettedImage)
imageForFormula config (EnvironmentOptions {..}) (FormulaOptions {..}) eqn =
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
                                [ "-density", show dpi
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

