Mutable State in Haskell
========================

> {-# LANGUAGE FlexibleContexts #-}
> module Mutable.Example where

Agenda
------

1. StateT
2. ST
3. IORef
4. MVar
5. TVar`
6. Something fun

> import Control.Monad
> import Control.Monad.State
> import Control.Monad.State.Class

Different approaches, each with its own use cases.

StateT
------

StateT is a monad transformer which supports a single piece of _pure mutable state_.

- _Pure_ - modelled using functions, not actual mutable variables.
- _Mutable_ - supports get and set operations.
- _Single state_ - We use `StateT` when we want read/write access to a single piece of mutable state.

Example
-------

Compute the Fibonacci numbers by keeping two integers as state:

> example1 :: State (Integer, Integer) Integer
> example1 = do
>   replicateM 100 $ do
>     (n, m) <- get
>     put (m, n + m)
>   (n, _) <- get
>   return n

Add logging using `StateT IO`:

> example2 :: StateT (Integer, Integer) IO Integer
> example2 = do
>   replicateM 100 $ do
>     (n, m) <- get
>     lift $ print n
>     put (m, n + m)
>   (n, _) <- get
>   return n 

We can abstract over the particular monad using the MonadState class:

> example3 :: (MonadState (Integer, Integer) m) => m Integer
> example3 = do
>   replicateM 100 $ do
>     (n, m) <- get
>     put (m, n + m)
>   (n, _) <- get
>   return n

