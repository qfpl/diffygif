{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Scratch where

import Control.Monad.Except

import Data.Default

import Text.Pandoc.Error
import Text.Pandoc.Options
import Text.Pandoc.Templates
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.LaTeX
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

-- load all code files in order :: [Content]
-- take diffs between each stop :: [[Content]]
-- need config options for language and the pauses between keypresses / stops
-- add markdown fences and language
-- styles for the code highlighting would be good as well

data Config =
  Config {
    cLanguage :: String
  , cKeyPause :: Int
  , cSectionPause :: Int
  , cBackground :: PixelRGB8
  }

instance Default Config where
  def = Config "haskell" 10 75 (PixelRGB8 255 255 255)

loadAndOrderFiles :: FilePath -> IO [String]
loadAndOrderFiles fp = do
  files <- listDirectory fp
  traverse (readFile . (fp <>)) (sort files)

past :: Diff a -> Maybe a
past (Both x _) = Just x
past (First x)  = Just x
past (Second _) = Nothing

pasts :: [Diff a] -> [a]
pasts = mapMaybe past

future :: Diff a -> Maybe a
future (Both _ x) = Just x
future (Second x) = Just x
future (First x)  = Nothing

futures :: [Diff a] -> [a]
futures = mapMaybe future

-- we should diff by lines first
-- then diff each line to simulate the typing

-- maybe try to add the newlines first
-- look at each chunked change, and if the number of lines is about to increase, add
-- a frame for the addition of each newline first?
diffs :: String -> String -> [String]
diffs s1 s2 =
  let
    diffs = getDiff s1 s2

    splits = fmap (\i -> splitAt i diffs) [0 .. length diffs]
    joins = fmap (\(x,y) -> futures x ++ pasts y) splits

    -- we can do much better than this in terms of running time
    -- a = fmap futures . inits $ diffs
    -- b = fmap pasts . tails $ diffs
    -- joins = zipWith (++) a b
  in
    map head . group $ joins

chunkify :: [String] -> [[String]]
chunkfiy [] = []
chunkify xs = zipWith diffs xs (tail xs)

palettedMap :: (forall pixel. Pixel pixel => Image pixel -> a) -> PalettedImage -> a 
palettedMap f (TrueColorImage d) = dynamicMap f d
palettedMap f (PalettedY8 i _) = f i
palettedMap f (PalettedRGB8 i _) = f i
palettedMap f (PalettedRGBA8 i _) = f i
palettedMap f (PalettedRGB16 i _) = f i

makeGif :: Config -> [[String]] -> ExceptT Error IO BL.ByteString
makeGif c pieces = do
  let
    f s = (markdownToLatex . codeToMarkdown c $ s) >>= latexToImage

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
    bits' = fmap modifyPause bits

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

markdownToLatex :: String -> ExceptT Error IO String
markdownToLatex s =
  let
     em = readMarkdown def s
  in do
    et <- liftIO $ getDefaultTemplate Nothing "latex"
    t <- case et of
      Left e -> throwError $ EPandocTemplate e
      Right s -> pure s
    case em of
      Left e -> throwError $ EPandoc e
      Right p -> pure $ writeLaTeX ( def { writerTemplate = Just t
                                         , writerHighlight = True
                                         , writerVariables  = [("header-includes", "\\pagestyle{empty}\n")]
                                         } ) p

latexToImage :: String -> ExceptT Error IO PalettedImage
latexToImage s = do
  ei <- liftIO $ imageForFormula defaultEnv ( FormulaOptions "" "" 200 ) s
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
imageForFormula :: EnvironmentOptions -> FormulaOptions -> Formula -> IO (Either RenderError PalettedImage)
imageForFormula (EnvironmentOptions {..}) (FormulaOptions {..}) eqn =
    bracket getCurrentDirectory setCurrentDirectory $ const $ withTemp $ \temp -> runExceptT $ do
  {-
      let doc = mconcat ["\\nonstopmode\n",
                 "\\documentclass[12pt]{article}\n",
                 "\\pagestyle{empty}\n", preamble,
                 "\\begin{document}\n",
                 eqn,
                 "\\end{document}\n"]
  -}
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
      (c'', o'', e'') <- io $ flip (readProcessWithExitCode imageMagickCommand) "" $
                                [ "-density", show dpi
                                , "-bordercolor", "none"
                                , "-border", "3x3"
                                -- , "-trim"
                                , "-type", "palette"
                                , "+antialias"
                                , "-background", "none"
                                -- , "-splice","1x0"
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

postprocess :: DynamicImage -> Either RenderError (Int, DynamicImage)
postprocess (ImageY8 i)     = second ImageY8     <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageY16 i)    = second ImageY16    <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYF i)     = second ImageYF     <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYA8 i)    = second ImageYA8    <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYA16 i)   = second ImageYA16   <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGB8 i)   = second ImageRGB8   <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGB16 i)  = second ImageRGB16  <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGBF i)   = second ImageRGBF   <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGBA8 i)  = second ImageRGBA8  <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGBA16 i) = second ImageRGBA16 <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYCbCr8 i) = second ImageYCbCr8 <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageCMYK8 i)  = second ImageCMYK8  <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageCMYK16 i) = second ImageCMYK16 <$> postprocess' i (pixelAt i 0 0)


postprocess' :: (Eq a, Pixel a) => Image a -> a -> Either RenderError (Int, Image a)
postprocess' img bg
  = do startX <- note ImageIsEmpty $ listToMaybe $ dropWhile isEmptyCol [0.. imageWidth img - 1]
       let (dotXs, postXs) = break isEmptyCol [startX .. imageWidth img]
       postX <- note CannotDetectBaseline $ listToMaybe postXs
       let postY = (+ 2) $ average $ dotXs >>= (\x -> takeWhile (not . isEmpty x) (dropWhile (isEmpty x) [0..imageHeight img - 1]))
           average = uncurry div . foldl' (\(s,c) e -> (e+s,c+1)) (0,0)
           newHeight = imageHeight img
           newWidth  = imageWidth img - postX + 3
           baseline  = imageHeight img - postY
       let image = generateImage (pixelAt' . (+ postX)) newWidth newHeight
       return (baseline, image)
  where
    isEmptyCol x = all (isEmpty x) [0.. imageHeight img - 1]
    isEmpty x = (== bg) . pixelAt img x
    pixelAt' x y | x < imageWidth img && y < imageHeight img = pixelAt img x y
                 | otherwise = bg
