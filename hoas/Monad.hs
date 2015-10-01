{-# LANGUAGE TupleSections #-}

module Main where

import Data.Maybe (mapMaybe)
import Data.String
import Control.Arrow (first)
import Control.Applicative
import Control.Monad

data FileType = File | Directory deriving (Show, Eq)

class (Monad m) => MonadFileSystem m where
  cd :: FilePath -> m ()
  ls :: m [(FilePath, FileType)]
  cat :: [FilePath] -> m String

joinFiles :: (MonadFileSystem m) => m String
joinFiles = do
  files <- ls
  cat (map fst files)

data FS = FS { files :: [(FilePath, String)], directories :: [(FilePath, FS)] } deriving (Show)

data Zipper = Zipper FS [FS]

data FakeFS a = FakeFS (Zipper -> (a, Zipper))

fake :: FakeFS a -> Zipper -> a
fake (FakeFS f) = fst . f

instance Functor FakeFS where
  fmap f (FakeFS g) = FakeFS $ first f . g

instance Applicative FakeFS where
  pure = return
  (<*>) = ap

instance Monad FakeFS where
  return a = FakeFS $ \z -> (a, z)
  FakeFS f >>= k = FakeFS $ \z0 ->
    let (a, z1)   = f z0
        FakeFS f1 = k a
    in f1 z1

instance MonadFileSystem FakeFS where
  cd "."  = return ()
  cd ".." = FakeFS $ \z@(Zipper _ ctx) ->
    case ctx of
      (up : ups) -> ((), Zipper up ups)
      _ -> ((), z)
  cd dir = FakeFS $ \z@(Zipper cur ups) ->
    case lookup dir (directories cur) of
      Just fs' -> ((), Zipper fs' (cur : ups))
      _ -> ((), z)
  ls = FakeFS $ \z@(Zipper cur _) ->
    (map ((, File) . fst) (files cur) ++ map ((, Directory) . fst) (directories cur), z)
  cat fs = FakeFS $ \z@(Zipper cur _) ->
    (unlines $ mapMaybe (\f -> lookup f (files cur)) fs, z)
