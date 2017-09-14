module Main where

import System.Environment

import Control.Monad.Except

import qualified Data.ByteString.Lazy as B

import Data.Default

import Scratch

main :: IO ()
main = do
  [fpIn, fpOut] <- getArgs
  s <- loadAndOrderFiles fpIn
  eb <- runExceptT $ makeGif def (chunkify s)
  case eb of
    Left e -> print e
    Right b -> B.writeFile fpOut b

