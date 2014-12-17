# Monads

## Primer - Algebraic Theories

Consider the `Identity` monad:

```haskell
newtype Identity a = { runIdentity :: a }
```

`Identity` is a `Monad`:

```haskell
instance Functor Identity where
  fmap f = Identity . f . runIdentity
  
instance Applicative Identity where
  pure = Identity
  (<*>) f x = Identity (runIdentity f (runIdentity x))
  
instance Monad Identity where
  return = Identity
  (>>=) f = f . runIdentity
```

This means that we can use `do` notation:

```haskell
addTwo :: (Num a) => Identity a -> Identity a -> Identity a
addTwo x y = do
  a <- x
  b <- y
  return (a + b)
```

Programming in this "extended" language is equivalent to programming in regular Haskell:

```haskell
addTwo :: (Num a) => a -> a -> a
addTwo x y =
  let a = x in
  let b = y in
  a + b
```

There is nothing new that we can do in the new language.

However, consider the `State` monad. We now have two new primitive operations, `get` and `put`, in the larger language which were not previously available:

```haskell
add :: (Num s) => s -> State s s
add s = do
  total <- get
  put (total + s)
  return total
```

We can think of this function as working in a larger language in which there are two new expressions `get` and `put x`:

```haskell
add :: (Num s) => s -> s
add s = do
  let total = get in
  put (total + s)
  total
```

## Theory

A category is a collection of objects ("types") and morphisms between objects ("functions"), supporting identities and composition:

```haskell
class Category k where
  id :: k a a
  (.) :: k b c -> k a b -> k a c
```

Think of categories as programming languages. Different refinements of the category concept exist, and give us different ways of talking about programming languages in the abstract:

- Categories with (co)products are programming languages with product types (sum types)
- Categories with (co)limits are programming languages with universal (existential) quantification

Every monad determines a category, the _Kleisli_ category for that monad:

```haskell
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Category (Klesili m) where
  id = Kleisli return
  (.) f g = Kleisli $ runKleisli f <=< runKleisli g
```

Put another way, every monad encodes the relationship between a programming language (`Hask`) and a larger programming language.

The additional operations of this larger language are the morphisms `Kleisli m a b` which are not equal to `return a` for any `a`. In other words, `return` embeds `Hask` into the larger language, but there is additional interesting stuff in the new language.

These are pure models of effects. We work in the larger programming language, but we need to project our functions down to the category `Hask` in order to see their effects. The way this is done depends on the monad in question.

## Intuition

- Categories are programming languages
- Monads encode embeddings of languages
- The Kleisli category extends the base category with new operations encoded by the monad

## Laws

The monad laws capture our expections of working in "larger languages".

## Examples

### `Identity`

- Morphisms are the same
- No new operations

`Identity` encodes the trivial embedding of a language into itself.

### `Maybe`

- Morphisms are partial functions
- Only new operation is `Nothing` which represents a missing value

`Maybe` encodes the process of replacing functions with partial functions. Alternatively, we extend our language with a new primitive called `Nothing`.

### `Either e`

- Morphisms are functions which can throw exceptions
- New operations are `Left x` for any `x`

`Either e` represents enlarging our language with exceptions of type `e`.

### `[]`

- Morphisms are multifunctions
- New operations are `[]` (failure) and any list with length `>= 2` (multiple successes).

`[]` encodes the process of replacing functions with multifunctions.

### `r -> -`

- Morphisms are the old morphisms, with the domain extended to include some type `r`
- New operation is `id`, which reads the configuration (a.k.a. `ask` in `Control.Monad.Reader`)

The "reader" monad endows our functions with read-access to some global configuration.

### `Writer w`

- Morphisms are the old morphisms, with the _codomain_ extended to include some type `w`
- New operation is `tell`, which writes a value to the log

The writer monad endows our functions with write-access to a global log.

### `State s`

- Morphisms are the old morphisms, with the _domain_ and _codomain_ extended to include some type `s`
- New operations are `get` and `put`, which read and write the state respectively

The state monad endows our functions with read and write access to a global state.

### `Cont r`

- Morphisms are morphisms in the old language, with the domain extended to include a callback:

    ```
    type Cont r a b = a -> (b -> r) -> r
                    = (a, b -> r) -> r
    ```
    
    The callback can be invoked zero or more times.
    
- The new operation is `callCC`, or "call with current continuation", which captures the callback as a function argument.

The continuation monad replaces the usual call stack with a tower of callbacks.

### Monad transformers

We want to be able to compose these programming language extensions. Monads do not compose in general in `Hask`, but many of the standard monads have corresponding _monad transformers_ which can be used to turn one monad into another. We are essentially stacking up new features to add to our base language.
