Mutable State in Haskell
========================

> {-# LANGUAGE FlexibleContexts #-}
> module Mutable.Example where

Agenda
------

1. StateT

> import Control.Monad
> import Control.Monad.State
> import Control.Monad.State.Class

2. IORef

> import Data.IORef

3. ST

> import Data.STRef
> import Data.Array
> import Data.Array.ST
> import Control.Monad.ST

4. MVar

> import Control.Concurrent
> import Control.Concurrent.MVar

5. TVar

> import System.Random (randomRIO)
> import Control.Concurrent.STM

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

IORef
-----

An IORef is a mutable reference which can be read to and written from in the IO monad. We can use multiple IORefs in the same piece of code.

IORefs are useful when we want to keep top-level global mutable state. For this reason, their use should be kept to a minimum.

Example
-------

> example4 :: IO Integer
> example4 = do
>   account1 <- newIORef 1000
>   account2 <- newIORef 200
>
>   transfer 500 account1 account2
>
>   readIORef account1
>   where
>   transfer :: Integer -> IORef Integer -> IORef Integer -> IO ()
>   transfer amt from to = do
>     modifyIORef from (\n -> n - amt)
>     modifyIORef to   (\n -> n + amt)

account1 and account2 have type IORef Integer. These values represent mutable references and can be passed around like ordinary values.

IORef is too powerful in many circumstances and restricts us to working in IO. Which motivates the use of:

ST
--

ST is like a restricted IO monad, which supports a restricted, pure form of mutation.

Mutable references have a scope (determined by the use of the runST function) and are not allowed to be modified or read outside that scope. This is enforced by the type system.

Instead of using IORef a, we use STRef s a.

Example
-------

Change IO to ST:

> example5 :: Integer
> example5 = runST $ do
>   account1 <- newSTRef 1000
>   account2 <- newSTRef 200
>
>   transfer 500 account1 account2
>
>   readSTRef account1
>   where
>   transfer :: Integer -> STRef s Integer -> STRef s Integer -> ST s ()
>   transfer amt from to = do
>     modifySTRef from (\n -> n - amt)
>     modifySTRef to   (\n -> n + amt)

The array package provides mutable arrays which can be used in the ST monad:

> example6 :: [a] -> [a]
> example6 xs = elems $ runSTArray $ do
>   arr <- newListArray (0, length xs - 1) xs
>   
>   lo <- newSTRef 0
>   hi <- newSTRef (length xs - 1)
>
>   let loop = do
>         i <- readSTRef lo
>         j <- readSTRef hi
>    
>         x <- readArray arr i
>         y <- readArray arr j
>         writeArray arr i y
>         writeArray arr j x
>
>         case j - i of
>           n | n <= 1 -> return arr
>             | otherwise -> do modifySTRef hi (\i -> i - 1)
>                               modifySTRef lo (\j -> j + 1)
>                               loop
>
>   loop

MVar
----

MVars are like IORefs, but are used to when we want to share mutable state between threads.

An MVar can be empty or full. A read on an empty MVar or a write on a full MVar will block the calling thread. This allows us to synchronize access across threads.

> example7 :: IO ()
> example7 = do
>   m <- newEmptyMVar
>
>   forkIO $ forever $ do
>     x <- takeMVar m
>     putStrLn x
>
>   replicateM_ 10 $ do
>     threadDelay 100000
>     putMVar m "Hello World!"

We can use MVars to build more interesting concurrency primitives. For example, channels and semaphores.

TVars
-----

TVars are transactional mutable variables which can be used in the STM monad.

Let's make the account example safe:

> example8 :: IO ()
> example8 = do
>   accounts <- mapM newTVarIO [100,200..500] 
>
>   replicateM_ 5 $ forkIO $ forever $ do
>     i <- randomRIO (0, 4)
>     j <- randomRIO (0, 4)    
>     transfer (accounts !! i) (accounts !! j) 100
>  
>   forever $ do
>     threadDelay 100000
>     amt <- readTVarIO (last accounts)
>     print amt
>   where
>   transfer :: TVar Integer -> TVar Integer -> Integer -> IO ()
>   transfer from to amt = atomically $ do
>     x <- readTVar from
>     when (x >= amt) $ do
>       writeTVar from (x - amt)
>       modifyTVar to (+ amt)

The Control.Concurrent.STM namespace provides lots of interesting transactional data structures which are built on top of TVars: channels, queues, etc. They are very useful for building safe multithreaded applications.

