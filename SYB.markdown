# Scrap Your Boilerplate!

See [here](http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/) for a list of the original SYB papers.

## Introduction

SYB is an approach to generic programming which uses type classes to simplify

Other similar libraries include `uniplate`, `multiplate`, `GHC.Generics`.

## Use Case

Generic programming libraries are appropriate when:

- Your data type has many data constructors
- You want to perform traversals, queries, transformations over your data

## Examples

- Optimization passes in a compiler (e.g. `hlint`)
- Generic zippers

## Motivating Example

I'll try to write a Javascript minifer using only `language-javascript` and `syb`.

## Transformations

- `everywhere` and `everywhere'` transform every occurence of a specific type inside a value of a different type.
- `somewhere` transforms at least one value inside another value.

`everywhere` has type:

    (forall a. Data a => a -> a) -> forall a. Data a => a -> a
    
Don't think about the types! Consider the examples:

    data Expr = Var String | Lam String Expr | App Expr Expr | Num Int
    
    varsToUpper :: Expr -> Expr
    varsToUpper = everywhere (mkT replace)
      where
      replace (Var s) = Var (map toUpper s)
      replace e = e
      
`mkT` makes a type-specific transformation function, and `everywhere` applies it everywhere inside a value of a different type.

`extT` can be used to extend a tranformation with more than one type-specific function.

    mkT f `extT` g `extT` h ...
      
Some other useful functions:

- `everywhereBut` allows you to specify which subterms should not be transformed using a generic query.
- `everywhereM` allows you to run a sub-computation with side-effects.

Example: Capture avoiding substitution

    subst :: String -> Expr -> Expr -> Expr
    subst v e = everywhereBut (mkQ False captured) (mkT replace)
      where
      captured (Lam v' _) | v == v' = True
      captured _ = False
      replace (Var v') | v == v' = e
      replace e = e

## Queries

- `everything` sums monoidal values over a structure
- `something` finds the first matching subterm

Example: Find all constants

    consts :: Expr -> [Int]
    consts = everything (mkQ (++) [] collect)
      where
      collect (Num n) = [n]
      collect _ = []
      
- `everythingWithContext` sums monoidal values while passing along a state      
      
Example: Find all free variables

    freeVars :: Expr -> [String]
    freeVars = everythingWithContext [] (++) (mkQ ((,) []) collect)
      where
      collect (Var v) bound | v `notElem` bound = ([v], bound)
      collect (Lam b _) bound = ([], b:bound)
      collect _ bound = ([], bound)
