# Constraint Satisfaction in Haskell

!

## Agenda

- Basic Haskell Intro
- Constraint Solving
- Extended Example + lots of code
- Questions

Please interrupt

!

## Haskell

Haskell is 

- Functional
- Statically typed
- Lazy
- General Purpose

!

## Resources

- Reddit Haskell
- Freenode IRC #haskell
- LA Haskell
- Books
  - Learn You A Haskell For Great Good!
  - Real World Haskell
  - Both available for free online

!

## Hello, Haskell!

```
main :: IO ()
main = putStrLn "Hello, World!"
```

!

## Hello, Haskell!

```
main :: IO ()
main = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"
```

!

## Hello, Haskell!

```
data TimeOfDay = Morning | Afternoon | Evening 

timeOfDay :: IO DayPart
timeOfDay :: ...

greeting :: TimeOfDay -> String -> String
greeting Morning name = 
  "Good Morning, " ++ name
greeting Afternoon name = 
  "Good Afternoon, " ++ name
greeting Evening name = 
  "Good Evening, " ++ name

main :: IO ()
main = do
  tod <- timeOfDay
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ greeting tod name
```

!

## Haskell

Haskell encourages

- Incremental, type-driven development
- Compositional reuse
- Abstraction

!

## Types

Functions

```
a -> b
a -> b -> c
(a -> b) -> c
```

!

## Types

Tuples

```
(a, b)
(a, b, c)
```

!

## Types

Algebraic Data Types

```
data Foo = Foo String | Bar Int
```

!

## Do Notation

Provides sequencing of computation

```
do x <- comp1
   y <- comp2 x
   return $ x + y
   
do x <- comp1
   let y = comp2 x
   return $ x + y
```

!

## Constraint Problems

Solve larger problems using local information

E.g. 

- Type Inference
- Logic Programming

!

## Constraint Problems

- Start with a set of constraints involving variables
- Solve by substitution

!

## Lambda Calculus

Terms are

- Abstractions (`λx. t`)
- Applications (`t1 t2`)
- Variables (`x`, `y`, `z`)

!

## Simply Typed Lambda Calculus

Types are

- Type variables (`A`, `B`, `C`)
- Functions (`t -> t`)

!

## Examples

```
λx. λy. x

λx. λy. λz. x z (y z)
```

How can we infer the types?

!

## Simply Typed Lambda Calculus

    data Tm = TmVar String
            | TmLam String Tm
            | TmApp Tm Tm

    data Ty = TyVar String
            | TyArr Ty Ty

!

## Type Inference

We would like a function

    typeOf :: Tm -> Ty

!

## Type Inference

But type inference can fail!

    typeOf :: Tm -> Maybe Ty

    data Maybe a = Nothing | Just a

!

## Side Effects

We need two types of side effects

- State
- Failure (Maybe)

We can create a new type which allows both types of side effect. Call it `Check`:

```
newtype Check = Check { ... }
```

!

## Side Effects

We'll use some basic values with side effects:

````
noType :: Check a

fresh :: Check Int
```

!

## Side Effects

And we can run side effects to get a result:

```
runCheck :: Check a -> Maybe a
```

`runCheck` forgets the final state, but can still fail

!

## Example

```
λx. λy. λz. x z (y z)
```

Notice:

```
typeOf x = TyArr (typeof z) ?
typeOf y = TyArr (typeof z) ?
```

Etc.

!

## Example

Generate unknowns for each variable

```
typeOf x = u1
typeOf y = u2
typeOf z = u3
```

!

## Example

```
u1 = TyArr u3 u4
u4 = TyArr u5 u6
u2 = TyArr u3 u5
```

---

!

## Example

```
u4 = TyArr u5 u6
u2 = TyArr u3 u5
```

---

```
u1 = TyArr u3 u4
```

!

## Example

```
u2 = TyArr u3 u5
```

---

```
u1 = TyArr u3 (TyArr u5 u6)
u4 = TyArr u5 u6
```

!

## Example

---

```
u1 = TyArr u3 (TyArr u5 u6)
u4 = TyArr u5 u6
u2 = TyArr u3 u5
```

!

## Example

Solution

    typeof t = forall u3 u5 u6. 
                 (u3 -> u5 -> u6) -> 
                 (u3 -> u5) -> 
                 u3 -> u6

!

## Unification

- Two structures are known to be equal 
- Different parts of each are missing
- Generates a set of constraints

E.g. if

```
((a -> b) -> c) = (d -> e -> f)
```

then we know

```
d = a -> b
```

and

```
c = e -> f
```

!

## Factoring

We want to split `typeOf` into

    collect :: Tm -> Check ([Constraint], Ty)
    solve :: [Constraint] -> Check Solution
    substitute :: Solution -> Ty -> Ty

!

## Constraints

    type Unknown = Int
    
    data Constraint = Constraint Unknown Ty

!

## Constraints

Modify

    data Ty = TyVar String
            | TyArr Ty Ty
            | TyUnk Unknown

!

## Solutions

    newtype Solution = Solution { 
        runSolution :: Unknown -> Ty 
      }

    substitute :: Solution -> Ty -> Ty
    substitute s (TyUnk u) = 
      runSolution s u
    substitute s (TyArr t1 t2) = 
      TyArr (substitute s t1)
            (substitute s t2)           
    substitute _ t = t

!

## High Level

    typeOf :: Tm -> Maybe Ty
    typeOf tm = runCheck $ do
      (cs, ty) <- collect tm
      sol <- solve cs
      return $ substitute sol ty

!

## collect

Collect constraints by *case analysis*

    collect :: Tm -> Check ([Constraint], Ty)

!

## collect

    import Data.Map

    type Env = Map String Unknown
	
	empty :: Env

    collect :: Tm -> Check [Constraint]
    collect = collect' empty
      where collect' :: Env -> 
                        Tm ->
                        Check ([Constraint], Ty)

!

## collect'

    collect' :: Env -> Tm -> 
                Check ([Constraint], Ty)
    collect' env (TmVar nm) = 
      case lookup nm env of
        Nothing -> noType
        Just u -> return ([], TyUnk u)

!

## collect'

    collect' :: Env -> Tm -> 
                Check ([Constraint], Ty)
    collect' env (TmAbs nm body) = do
      u <- fresh
      collect' (M.insert nm u env) body

!

## collect'

    collect' :: Env -> Tm -> 
                Check [Constraint]
    collect' env (TmApp tm1 tm2) = do
      (cs1, ty1) <- collect' env tm1
      (cs2, ty2) <- collect' env tm2
      u1 <- fresh
      -- ty1 is a function type
      let cs3 = [Constraint u1 (TyArr u2 u3), Constraint u1 ty1]
      -- ty2 matches ty1
      let cs4 = [Constraint u2 ty2]
      return (join [cs1, cs2, cs3, cs4], TyUnk u3)

!

## solve

    bottom :: Solution
    bottom = Solution TyUnk

    solve :: [Constraint] -> Check Solution
    solve = refine bottom
      where
      refine :: Solution -> [Constraint] ->
                Check Solution

!

## refine

    refine s [] = return s
    refine (c:cs) = do
      let s' = replace c . s
      cs' <- replaceC c cs
      refine s' cs'

!

## replace

    replace :: Constraint -> Ty -> Ty
    replace (Constraint u t) (TyUnk u1) | u == u1 = t
    replace c (TyArr t1 t2) = TyArr (replace c t1) (replace c t2)
    replace _ t = t

!

## replaceC

    replaceC :: Constraint -> 
                [Constraint] -> 
                Check [Constraint]
    replaceC _ [] = Just []
    replaceC c@(Constraint u t) (Constraint u1 t1 : cs) 
      | u == u1 = (++) <$> unify t t1 <*> replaceC c cs
      | otherwise = (:) <$> pure (Constraint u1 (replace c t1)) 
                        <*> replaceC c cs

!

## unify

    unify :: Ty -> Ty -> Check [Constraint]
    unify (TyVar v1) (TyVar v2) 
      | v1 == v2 = Just []
    unify (TyArr t1 t2) (TyArr t3 t4) = 
      (++) <$> unify t1 t3 
           <*> unify t2 t4 
    unify (TyUnk u) t = do
      occursCheck u t
      return [Constraint u t]
    unify t1 t2@(TyUnk u) = 
      unify t2 t1
    unify _ _ = noType

!

## occursCheck

    occursCheck :: Unknown -> Ty -> Check ()
    occursCheck u (TUnk u1) 
      | u == u1 = return ()
    occursCheck u t = go u t
      where
      go u (TyUnk u1) 
        | u == u1 = noType
      go u (TyArr t1 t2) = do
        occursCheck u t1 
        occursCheck u t2
      go _ _ = return ()

!

## More?

- Generics
- `Foldable`, `Traversable`
- `Maybe` vs. `Either`
- `KanrenT`
- Generalize over `F`

!

## Questions?

