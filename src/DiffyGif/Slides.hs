{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module DiffyGif.Slides (
    Slide(..)
  , slideContent
  , readSlides
  )
  where

import Text.Read (readMaybe)

import Control.Monad.Trans (liftIO)
import Control.Monad.Except (ExceptT, throwError)

import DiffyGif.Error

data Slide a =
    Display Int a
  | Type a
  deriving (Eq, Ord, Show)

slideContent ::
  Slide a ->
  a
slideContent (Display _ x) =
  x
slideContent (Type x) =
  x

parseSlideMeta ::
  Monad m =>
  String ->
  ExceptT Error m (Slide FilePath)
parseSlideMeta s =
  case words s of
    ["DISPLAY", i, fp] ->
      case readMaybe i of
        Nothing ->
          throwError $ EParse "expected an int"
        Just j ->
          pure $ Display j fp
    ["TYPE", fp] ->
      pure $ Type fp
    _ ->
      throwError $ EParse "unknown line"

parseSlidesMeta ::
  Monad m =>
  String ->
  ExceptT Error m [Slide FilePath]
parseSlidesMeta =
  traverse parseSlideMeta . lines

readSlidesMeta ::
  FilePath ->
  ExceptT Error IO [Slide FilePath]
readSlidesMeta fp = do
  s <- liftIO $ readFile fp
  parseSlidesMeta s

loadSlide ::
  Slide FilePath ->
  IO (Slide String)
loadSlide (Display i fp) =
  Display i <$> readFile fp
loadSlide (Type fp) =
  Type <$> readFile fp

loadSlides ::
  [Slide FilePath] ->
  IO [Slide String]
loadSlides =
  traverse loadSlide

readSlides ::
  FilePath ->
  ExceptT Error IO [Slide String]
readSlides fp =
  liftIO . loadSlides =<< readSlidesMeta fp
