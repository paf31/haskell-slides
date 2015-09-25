> module Main where

> import Control.Applicative

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
>   fmap f (MatchRoute g) = MatchRoute $ fmap (\(a, ss) -> (f a, ss)) . g

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