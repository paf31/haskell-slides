# Monoids

## Motivation

Suppose we want to concatenate a list of strings:

```haskell
concatenate :: [String] -> String
```

We can implement this using a fold:

```haskell
concatenate = foldl (++) ""
```

But this function is more general, and works for more than just `String`s.

We generalize `(++)` to a function `mappend` and the empty string to an "empty value" `mempty`:

```haskell
concatenate' = foldl mappend mempty
```

Ask `ghci` for the type:

```text
Prelude> import Data.Monoid 
Prelude Data.Monoid> :t foldl mappend mempty

foldl mappend mempty :: Monoid a => [a] -> a
```

In fact, `concatenate` is just `mconcat` from `Data.Monoid`.

## Monoid Type Class

A `Monoid` is a type with an associative binary operation (`mappend`) and an empty value (`mempty`).

```haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
```

## Examples

Strings are monoids, as we have seen:

```haskell
instance Monoid String where
  mempty = ""
  mappend = (++)
```

So are lists, with the same instance!

```haskell
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

Any instance of `Num` gives an instance of `Monoid` in at least two ways:

```haskell
newtype Sum a = Sum a

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum n) (Sum m) = Sum (n + m)

newtype Product a = Product a

instance (Num a) => Monoid (Product a) where
  mempty = Product 1
  mappend (Product n) (Product m) = Product (n * m)
```

For any type `a`, there is a type of _endomorphisms_ of `a` (functions from `a` to `a`):

```haskell
newtype Endo a = Endo (a -> a)
```
 
`Endo a` forms a `Monoid` for any type `a`:

```haskell
instance Monoid (Endo a) where
  mempty = Endo id
  mappend (Endo f) (Endo g) = Endo (f . g)
```

E.g.

```haskell
numberYouFirstThoughtOf = 
  concatenate [ \n -> n / 2
              , \n -> n - 6
              , \n -> n * 2
              , \n -> n + 3
              ]
```

## Laws

Not just any `mempty` and `mappend` will do. The instance must satisfy _laws_:

- Associativity: `forall x y z. mappend (x (mappend y z)) = mappend (mappend (x y) z)`
- Left zero: `forall x. mappend mempty x = x`
- Right zero: `forall x. mappend x mempty = x`

Laws allow us to reason about code equationally in the abstract.

For example: prove that

```
forall xs ys. mconcat (xs ++ ys) = mappend (mconcat xs) (mconcat ys)
```

Break `xs` into cases:

```haskell
mappend (mconcat []) (mconcat ys) 
  {- Definition of mconcat -}
  = mappend mempty (mconcat ys)
  {- Left zero law -}
  = mconcat ys
  {- Definition of (++) -}
  = mconcat ([] ++ ys)
```

When `xs` is non-empty:

```haskell
mappend (mconcat (x : xs)) (mconcat ys)
  {- Definition of mconcat -}
  = mappend (mappend x (mconcat xs)) (mconcat ys)
  {- Associativity -}
  = mappend x (mappend (mconcat xs) (mappend ys))
  {- Inductive hypothesis -}
  = mappend x (mconcat (xs ++ ys))
  {- Definition of mconcat -}
  = mconcat (x : xs ++ ys)
  {- Definition of (++) -}
  = mconcat ((x : xs) ++ ys)
```

## Semigroups

Semigroups are like monoids without the requirement to have an empty value:

```haskell
class Semigroup s where
  sappend :: s -> s -> s
```

Also, the zero laws no longer hold (they don't make sense).

Fewer requirements implies:

- More examples
- Fewer functions

E.g. non-empty lists are not monoids but they are semigroups.

No `mconcat`, but we can concatenate non-empty lists of elements:

```haskell
sconcat :: (Semigroup s) => NonEmpty s -> s
```

Are semigroups useful? Yes! E.g. applicative validation.

# IO

## Modelling the World

We can model IO using the `Endo` monoid:

```haskell
type Console = [String]

data World = World Console

type IO = Endo World

runIO :: [String] -> IO -> [String]
runIO stdin io = 
  case runEndo io (World []) of
    World stdout -> stdout
```

We can add more capabilities to our `World` type as we need them.

## Adding Return Types

If we want to include the return type of our actions in their types, we might use a model like this:

```haskell
data IO a
  = Pure a
  | PutStrLn String (IO a)
  | GetStrLn (String -> IO a)
```

We can write "programs" in this model:

```haskell
cat :: IO a
cat = GetStrLn $ \s -> PutStrLn s $ cat
```

Notice that a value of type `IO a` does not necessarily contain a value of type `a`.

## Sequencing Actions

To use `do` notation to sequence actions, we need an instance of the `Monad` type class:

```haskell
instance Monad IO where
  return = Pure
  (>>=) (PutStrLn s k) f = PutStrLn s (k >>= f)
  (>>=) (GetStrLn k) f = GetStrLn (\s -> k s >>= f)
```

Need to make sure this respects the laws.

Now we can write:

```haskell
cat :: IO a
cat = do
  s <- GetStrLn Pure
  PutStrLn s (Pure ())
  cat
```

Or define helper functions:

```haskell
getStrLn :: IO String
getStrLn = GetStrLn Pure

putStrLn :: String -> IO ()
putStrLn s = PutStrLn s (Pure ())
```

And write:

```haskell
cat :: IO a
  s <- getStrLn
  putStrLn s
  cat
```

## Interpreting the Monad

GHC's `IO` is like our `World` model of IO, but where the monad can only be interpreted _by the Haskell runtime_.

# Functors


