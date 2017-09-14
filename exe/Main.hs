module Main where

import System.Environment

import Control.Monad.Except

import qualified Data.ByteString.Lazy as B

import Scratch

main :: IO ()
main = do
  [fpIn, fpOut] <- getArgs
  s <- readFile fpIn
  eb <- runExceptT $ markdownToLatex s >>= latexToImage
  case eb of
    Left e -> print e
    Right b -> B.writeFile fpOut b

