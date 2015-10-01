{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Main where

import Data.Maybe (mapMaybe)
import Data.String
import Control.Applicative
import Control.Monad

class (IsString (f ()), Applicative f) => Route f where
  match :: f String

newtype PrintRoute a = PrintRoute { printRoute :: Int -> ([String], Int) }

renderRoute :: PrintRoute a -> String
renderRoute r = concatMap ('/' :) . fst $ printRoute r 0

instance Functor PrintRoute where
  fmap _ (PrintRoute f) = PrintRoute f

instance Applicative PrintRoute where
  pure _ = PrintRoute $ \i -> ([], i)
  PrintRoute f <*> PrintRoute g = PrintRoute $ \i ->
    let (ss1, j) = f i
        (ss2, k) = g j
    in (ss1 ++ ss2, k)

instance (a ~ ()) => IsString (PrintRoute a) where
  fromString s = PrintRoute $ \i -> ([s], i)

instance Route PrintRoute where
  match = PrintRoute $ \i -> ([":match" ++ show i], i + 1)

newtype MatchRoute a = MatchRoute { runMatchRoute :: [String] -> Maybe (a, [String]) }

matchRoute :: MatchRoute a -> String -> Maybe a
matchRoute r = fmap fst . runMatchRoute r . filter (not . null) . split []
  where
  split [] [] = []
  split acc [] = [acc]
  split acc ('/' : s) = acc : split [] s
  split acc (c : s) = split (acc ++ [c]) s

instance Functor MatchRoute where
  fmap f (MatchRoute g) = MatchRoute $ fmap (first f) . g

instance Applicative MatchRoute where
  pure a = MatchRoute $ \ss -> Just (a, ss)
  MatchRoute f <*> MatchRoute g = MatchRoute $ \ss0 -> do
    (a, ss1) <- f ss0
    (b, ss2) <- g ss1
    return (a b, ss2)

instance (a ~ ()) => IsString (MatchRoute a) where
  fromString s = MatchRoute $ \ss ->
    case ss of
      (s1 : ss1) | s1 == s -> Just ((), ss1)
      _ -> Nothing

instance Route MatchRoute where
  match = MatchRoute $ \ss ->
    case ss of
      (s : ss1) -> Just (s, ss1)
      _ -> Nothing
