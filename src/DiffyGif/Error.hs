module DiffyGif.Error (
    Error(..)
  , RenderError(..)
  ) where

import Control.Exception

import Codec.Picture.Types
import Text.Pandoc.Error

data Error =
    EPandocTemplate IOException
  | EPandoc PandocError
  | ERender RenderError
  | EImage String
  | EParse String
  deriving Show

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
