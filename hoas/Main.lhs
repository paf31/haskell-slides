> {-# LANGUAGE FlexibleInstances, GADTs, TupleSections #-}

> module Main where

> import Data.Maybe (mapMaybe)
> import Control.Arrow (first)
> import Control.Applicative
> import Control.Monad

Example 1: Monoids - Pretty Printing

> class (Monoid a) => Document a where
>   text :: String -> a
>   indent :: Int -> a -> a
>   beside :: a -> a -> a

> data PlainText = PlainText { docWidth :: Int, docLines :: [String] } deriving (Show)

> render :: PlainText -> String
> render = unlines . docLines

> instance Monoid PlainText where
>   mempty = PlainText 0 []
>   mappend (PlainText w1 l1) (PlainText w2 l2) = PlainText (max w1 w2) (l1 ++ l2)

> instance Document PlainText where
>   text s = PlainText (length s) [s]
>   indent n (PlainText w s) = PlainText (w + n) (map ((replicate n ' ') ++) s)
>   beside (PlainText w1 l1) (PlainText w2 l2) = PlainText (w1 + w2) lines
>     where
>     lines = take lineCount $
>       zipWith (++) (map (pad w1) l1 ++ repeat (emptyLine w1))
>                    (map (pad w2) l2 ++ repeat (emptyLine w2))
> 
>     pad w = take w . (++ (repeat ' '))
> 
>     emptyLine w = replicate w ' '
> 
>     lineCount = max (length l1) (length l2)

Example 2: Applicative - Parsing Routes

> class (Applicative f) => Route f where
>   lit :: String -> f ()
>   match :: f String
  
> newtype PrintRoute a = PrintRoute { printRoute :: Int -> ([String], Int) }

> renderRoute :: PrintRoute a -> String
> renderRoute r = concat . map ('/' :) . fst $ printRoute r 0

> instance Functor PrintRoute where 
>   fmap _ (PrintRoute f) = PrintRoute f

> instance Applicative PrintRoute where
>   pure _ = PrintRoute $ \i -> ([], i)
>   PrintRoute f <*> PrintRoute g = PrintRoute $ \i -> 
>     let (ss1, j) = f i 
>         (ss2, k) = g j
>     in (ss1 ++ ss2, k)

> instance Route PrintRoute where
>   lit s = PrintRoute $ \i -> ([s], i)
>   match = PrintRoute $ \i -> ([":match" ++ show i], i + 1)

> newtype MatchRoute a = MatchRoute { runMatchRoute :: [String] -> Maybe (a, [String]) }

> matchRoute :: MatchRoute a -> String -> Maybe a
> matchRoute r = fmap fst . runMatchRoute r . filter (not . null) . split []
>   where
>   split [] [] = []
>   split acc [] = [acc]
>   split acc ('/' : s) = acc : split [] s
>   split acc (c : s) = split (acc ++ [c]) s

> instance Functor MatchRoute where 
>   fmap f (MatchRoute g) = MatchRoute $ fmap (first f) . g

> instance Applicative MatchRoute where
>   pure a = MatchRoute $ \ss -> Just (a, ss)
>   MatchRoute f <*> MatchRoute g = MatchRoute $ \ss0 -> do
>     (a, ss1) <- f ss0
>     (b, ss2) <- g ss1
>     return (a b, ss2)

> instance Route MatchRoute where
>   lit s = MatchRoute $ \ss -> 
>     case ss of
>       (s1 : ss1) | s1 == s -> Just ((), ss1)
>       _ -> Nothing
>   match = MatchRoute $ \ss -> 
>     case ss of
>       (s : ss1) -> Just (s, ss1)
>       _ -> Nothing

Example 3: Monad - Virtual Filesystem

> data FileType = File | Directory deriving (Show, Eq)
>
> class (Monad m) => MonadFileSystem m where
>   cd :: FilePath -> m ()
>   ls :: m [(FilePath, FileType)]
>   cat :: [FilePath] -> m String

> joinFiles :: (MonadFileSystem m) => m String
> joinFiles = do
>   files <- ls
>   cat (map fst files)

> data FS = FS { files :: [(FilePath, String)], directories :: [(FilePath, FS)] } deriving (Show)

> data Zipper = Zipper FS [FS]

> data FakeFS a = FakeFS (Zipper -> (a, Zipper))

> fake :: FakeFS a -> Zipper -> a
> fake (FakeFS f) = fst . f

> instance Functor FakeFS where 
>   fmap f (FakeFS g) = FakeFS $ first f . g

> instance Applicative FakeFS where
>   pure = return 
>   (<*>) = ap

> instance Monad FakeFS where
>   return a = FakeFS $ \z -> (a, z)
>   FakeFS f >>= k = FakeFS $ \z0 -> 
>     let (a, z1)   = f z0
>         FakeFS f1 = k a
>     in f1 z1

> instance MonadFileSystem FakeFS where
>   cd "."  = return ()
>   cd ".." = FakeFS $ \z@(Zipper _ ctx) -> 
>     case ctx of
>       (up : ups) -> ((), Zipper up ups)
>       _ -> ((), z)
>   cd dir = FakeFS $ \z@(Zipper cur ups) -> 
>     case lookup dir (directories cur) of
>       Just fs' -> ((), Zipper fs' (cur : ups))
>       _ -> ((), z)
>   ls = FakeFS $ \z@(Zipper cur _) -> 
>     (map ((, File) . fst) (files cur) ++ map ((, Directory) . fst) (directories cur), z)
>   cat fs = FakeFS $ \z@(Zipper cur _) -> 
>     (unlines $ mapMaybe (\f -> lookup f (files cur)) fs, z)

Example 4: Higher-Order Abstract Syntax

> class HOAS f where
>   ($$) :: f (a -> b) -> f a -> f b
>   lam :: (f a -> f b) -> f (a -> b)

> konst :: (HOAS f) => f (a -> b -> a)
> konst = lam $ \a -> lam $ \_ -> a

> app :: (HOAS f) => f (a -> (a -> b) -> b)
> app = lam $ \a -> lam $ \f -> f $$ a

> data PPrint a = PPrint { prettyPrint :: Int -> String, atomic :: Bool }

> instance HOAS PPrint where
>   PPrint f at1 $$ PPrint g at2 = PPrint (\i -> parens at1 (f i) ++ " $$ " ++ parens at2 (g i)) False
>     where
>     parens True  = id
>     parens False = ("(" ++) . (++ ")")
>   lam f = PPrint (\i -> "lam $ \a" ++ show i ++ " -> " ++ prettyPrint (f (PPrint (\_ -> "a" ++ show i) True)) (i + 1)) False
