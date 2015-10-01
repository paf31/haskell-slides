module Main where

class HOAS f where
  ($$) :: f (a -> b) -> f a -> f b
  lam :: (f a -> f b) -> f (a -> b)

konst :: (HOAS f) => f (a -> b -> a)
konst = lam $ \a -> lam $ const a

app :: (HOAS f) => f (a -> (a -> b) -> b)
app = lam $ \a -> lam $ \f -> f $$ a

data PPrint a = PPrint { prettyPrint :: Int -> String, atomic :: Bool }

instance HOAS PPrint where
  PPrint f at1 $$ PPrint g at2 = PPrint (\i -> parens at1 (f i) ++ " $$ " ++ parens at2 (g i)) False
    where
    parens True  = id
    parens False = ("(" ++) . (++ ")")
  lam f = PPrint (\i -> "lam $ \a" ++ show i ++ " -> " ++ prettyPrint (f (PPrint (\_ -> "a" ++ show i) True)) (i + 1)) False
