> module Main where

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