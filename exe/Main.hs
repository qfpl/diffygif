module Main where

import System.Environment

import Control.Monad.Except

import qualified Data.ByteString.Lazy as B

import Data.Default

import DiffyGif.Gif
import DiffyGif.Diff
import DiffyGif.Slides

main :: IO ()
main = do
  [fpIn, fpOut] <- getArgs
  eb <- runExceptT $ do
    s <- readSlides fpIn
    makeGif def (slidesDiff def s)
  case eb of
    Left e -> print e
    Right b -> B.writeFile fpOut b

