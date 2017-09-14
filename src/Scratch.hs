{-# LANGUAGE RecordWildCards #-}
module Scratch where

import Control.Monad.Except

import Data.Default

import Text.Pandoc.Error
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.LaTeX
import Codec.Picture.Png
import qualified Data.ByteString.Lazy as B

-- from image latex render
import Codec.Picture
import Data.Maybe
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
import Data.Monoid

data Error =
    EPandoc PandocError
  | ERender RenderError
  | EImage String
  deriving Show

-- load all code files in order :: [Content]
-- take diffs between each stop :: [[Content]]
-- need config options for language and the pauses between keypresses / stops
-- add markdown fences and language
-- would be nice to be able to get hold of the preamble programmatically
-- styles for the code highlighting would be good as well

markdownToLatex :: String -> ExceptT Error IO String
markdownToLatex s =
  let
     em = readMarkdown def s
  in
    case em of
      Left e -> throwError $ EPandoc e
      Right p -> pure $ writeLaTeX ( def { writerHighlight = True } ) p

latexToImage :: String -> ExceptT Error IO B.ByteString
latexToImage s = do
  ei <- liftIO $ imageForFormula defaultEnv ( FormulaOptions pre "chapter" 200 ) s
  di <- case ei of
          Left e -> throwError $ ERender e
          Right (_, x) -> pure x
  case encodeDynamicPng di of
    Left e -> throwError $ EImage s
    Right x -> pure x

pre :: String
pre = unlines
    [ "\\usepackage{lmodern}",
    "\\usepackage{amssymb,amsmath}",
    "\\usepackage{ifxetex,ifluatex}",
    "\\usepackage{fixltx2e} % provides \\textsubscript",
    "\\ifnum 0\\ifxetex 1\\fi\\ifluatex 1\\fi=0 % if pdftex",
    "  \\usepackage[T1]{fontenc}",
    "  \\usepackage[utf8]{inputenc}",
    "\\else % if luatex or xelatex",
    "  \\ifxetex",
    "    \\usepackage{mathspec}",
    "  \\else",
    "    \\usepackage{fontspec}",
    "  \\fi",
    "  \\defaultfontfeatures{Ligatures=TeX,Scale=MatchLowercase}",
    "\\fi",
    "% use upquote if available, for straight quotes in verbatim environments",
    "\\IfFileExists{upquote.sty}{\\usepackage{upquote}}{}",
    "% use microtype if available",
    "\\IfFileExists{microtype.sty}{%",
    "\\usepackage[]{microtype}",
    "\\UseMicrotypeSet[protrusion]{basicmath} % disable protrusion for tt fonts",
    "}{}",
    "\\PassOptionsToPackage{hyphens}{url} % url is loaded by hyperref",
    "\\usepackage[unicode=true]{hyperref}",
    "\\hypersetup{",
    "            pdfborder={0 0 0},",
    "            breaklinks=true}",
    "\\urlstyle{same}  % don't use monospace font for urls",
    "\\usepackage{color}",
    "\\usepackage{fancyvrb}",
    "\\newcommand{\\VerbBar}{|}",
    "\\newcommand{\\VERB}{\\Verb[commandchars=\\\\\\{\\}]}",
    "\\DefineVerbatimEnvironment{Highlighting}{Verbatim}{commandchars=\\\\\\{\\}}",
    "% Add ',fontsize=\\small' for more characters per line",
    "\\newenvironment{Shaded}{}{}",
    "\\newcommand{\\KeywordTok}[1]{\\textcolor[rgb]{0.00,0.44,0.13}{\\textbf{#1}}}",
    "\\newcommand{\\DataTypeTok}[1]{\\textcolor[rgb]{0.56,0.13,0.00}{#1}}",
    "\\newcommand{\\DecValTok}[1]{\\textcolor[rgb]{0.25,0.63,0.44}{#1}}",
    "\\newcommand{\\BaseNTok}[1]{\\textcolor[rgb]{0.25,0.63,0.44}{#1}}",
    "\\newcommand{\\FloatTok}[1]{\\textcolor[rgb]{0.25,0.63,0.44}{#1}}",
    "\\newcommand{\\ConstantTok}[1]{\\textcolor[rgb]{0.53,0.00,0.00}{#1}}",
    "\\newcommand{\\CharTok}[1]{\\textcolor[rgb]{0.25,0.44,0.63}{#1}}",
    "\\newcommand{\\SpecialCharTok}[1]{\\textcolor[rgb]{0.25,0.44,0.63}{#1}}",
    "\\newcommand{\\StringTok}[1]{\\textcolor[rgb]{0.25,0.44,0.63}{#1}}",
    "\\newcommand{\\VerbatimStringTok}[1]{\\textcolor[rgb]{0.25,0.44,0.63}{#1}}",
    "\\newcommand{\\SpecialStringTok}[1]{\\textcolor[rgb]{0.73,0.40,0.53}{#1}}",
    "\\newcommand{\\ImportTok}[1]{#1}",
    "\\newcommand{\\CommentTok}[1]{\\textcolor[rgb]{0.38,0.63,0.69}{\\textit{#1}}}",
    "\\newcommand{\\DocumentationTok}[1]{\\textcolor[rgb]{0.73,0.13,0.13}{\\textit{#1}}}",
    "\\newcommand{\\AnnotationTok}[1]{\\textcolor[rgb]{0.38,0.63,0.69}{\\textbf{\\textit{#1}}}}",
    "\\newcommand{\\CommentVarTok}[1]{\\textcolor[rgb]{0.38,0.63,0.69}{\\textbf{\\textit{#1}}}}",
    "\\newcommand{\\OtherTok}[1]{\\textcolor[rgb]{0.00,0.44,0.13}{#1}}",
    "\\newcommand{\\FunctionTok}[1]{\\textcolor[rgb]{0.02,0.16,0.49}{#1}}",
    "\\newcommand{\\VariableTok}[1]{\\textcolor[rgb]{0.10,0.09,0.49}{#1}}",
    "\\newcommand{\\ControlFlowTok}[1]{\\textcolor[rgb]{0.00,0.44,0.13}{\\textbf{#1}}}",
    "\\newcommand{\\OperatorTok}[1]{\\textcolor[rgb]{0.40,0.40,0.40}{#1}}",
    "\\newcommand{\\BuiltInTok}[1]{#1}",
    "\\newcommand{\\ExtensionTok}[1]{#1}",
    "\\newcommand{\\PreprocessorTok}[1]{\\textcolor[rgb]{0.74,0.48,0.00}{#1}}",
    "\\newcommand{\\AttributeTok}[1]{\\textcolor[rgb]{0.49,0.56,0.16}{#1}}",
    "\\newcommand{\\RegionMarkerTok}[1]{#1}",
    "\\newcommand{\\InformationTok}[1]{\\textcolor[rgb]{0.38,0.63,0.69}{\\textbf{\\textit{#1}}}}",
    "\\newcommand{\\WarningTok}[1]{\\textcolor[rgb]{0.38,0.63,0.69}{\\textbf{\\textit{#1}}}}",
    "\\newcommand{\\AlertTok}[1]{\\textcolor[rgb]{1.00,0.00,0.00}{\\textbf{#1}}}",
    "\\newcommand{\\ErrorTok}[1]{\\textcolor[rgb]{1.00,0.00,0.00}{\\textbf{#1}}}",
    "\\newcommand{\\NormalTok}[1]{#1}",
    "\\IfFileExists{parskip.sty}{%",
    "\\usepackage{parskip}",
    "}{% else",
    "\\setlength{\\parindent}{0pt}",
    "\\setlength{\\parskip}{6pt plus 2pt minus 1pt}",
    "}",
    "\\setlength{\\emergencystretch}{3em}  % prevent overfull lines",
    "\\providecommand{\\tightlist}{%",
    "  \\setlength{\\itemsep}{0pt}\\setlength{\\parskip}{0pt}}",
    "\\setcounter{secnumdepth}{0}",
    "% Redefines (sub)paragraphs to behave more like sections",
    "\\ifx\\paragraph\\undefined\\else",
    "\\let\\oldparagraph\\paragraph",
    "\\renewcommand{\\paragraph}[1]{\\oldparagraph{#1}\\mbox{}}",
    "\\fi",
    "\\ifx\\subparagraph\\undefined\\else",
    "\\let\\oldsubparagraph\\subparagraph",
    "\\renewcommand{\\subparagraph}[1]{\\oldsubparagraph{#1}\\mbox{}}",
    "\\fi",
    "",
    "% set default figure placement to htbp",
    "\\makeatletter",
    "\\def\\fps@figure{htbp}",
    "\\makeatother"]
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
imageForFormula :: EnvironmentOptions -> FormulaOptions -> Formula -> IO (Either RenderError (Baseline, DynamicImage))
imageForFormula (EnvironmentOptions {..}) (FormulaOptions {..}) eqn =
    bracket getCurrentDirectory setCurrentDirectory $ const $ withTemp $ \temp -> runExceptT $ do
      let doc = mconcat ["\\nonstopmode\n",
                 "\\documentclass[12pt]{article}\n",
                 "\\pagestyle{empty}\n", preamble,
                 "\\begin{document}\n",
                 eqn,
                 "\\end{document}\n"]
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
                                , "-border", "1x1"
                                , "-trim"
                                , "-background", "none"
                                , "-splice","1x0"
                                ] ++ imageMagickArgs ++
                                [ tempFileBaseName <.> "ps", tempFileBaseName <.> "png" ]
      io $ removeFile (tempFileBaseName <.> "ps")
      when (c'' /= ExitSuccess) $ throwE $ IMConvertFailure (o'' ++ "\n" ++ e'')
      imgM <- io $ readImage (tempFileBaseName <.> "png")
      img <- withExceptT ImageReadError $ hoistEither imgM
      io $ removeFile $ tempFileBaseName <.> "png"
      -- hoistEither $ postprocess img
      pure (0, img)
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
