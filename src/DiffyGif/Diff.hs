{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module DiffyGif.Diff (
    DiffConfig(..)
  , slidesDiff
  ) where

import Data.List (group)

import Data.Default

import Data.Algorithm.Diff

import DiffyGif.Slides

data DiffConfig =
  DiffConfig {
    dcKeyPause :: Int
  , dcSectionPause :: Int
  }

instance Default DiffConfig where
  def = DiffConfig 10 75

past ::
  Diff a ->
  Maybe a
past (Both x _) =
  Just x
past (First x) =
  Just x
past (Second _) =
  Nothing

future ::
  Diff a ->
  Maybe a
future (Both _ x) =
  Just x
future (Second x) =
  Just x
future (First x) =
  Nothing

diffs ::
  Eq a =>
  [a] ->
  [a] ->
  [[a]]
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

interdiffs ::
  Eq a =>
  [[a]] ->
  [[[a]]]
interdiffs [] =
  []
interdiffs xs =
  zipWith diffs xs (tail xs)

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

addPauses ::
  DiffConfig ->
  [[String]] ->
  [(Int, String)]
addPauses c xs =
  let
    f [] = []
    f (h : t) =
      (dcSectionPause c, h) : fmap (\s -> (dcKeyPause c, s)) t
  in
    foldMap f xs

slidesDiff ::
  DiffConfig ->
  [Slide String] ->
  [(Int, String)]
slidesDiff c xs =
  slidesDiff' c (Display 0 "") xs

slidesDiff' ::
  DiffConfig ->
  Slide String ->
  [Slide String] ->
  [(Int, String)]
slidesDiff' c _ [] =
  []
slidesDiff' c pre (h@(Display i s) : t) =
  (i, s) : slidesDiff' c h t
slidesDiff' c pre (h@(Type s) : t) =
  addPauses c (chunkify [slideContent pre, s]) ++ slidesDiff' c h t
