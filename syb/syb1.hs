{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}

import Data.Data
import Data.Generics
import Data.Char
import qualified Data.Map as M

data Expr
  = Var String
  | Lam String Expr 
  | App Expr Expr 
  | Num Int deriving (Show, Data, Typeable)
    
varsToUpper :: Expr -> Expr 
varsToUpper = everywhere (mkT replace)
      where
      replace (Var s) = Var (map toUpper s)
      replace e = e

subst :: String -> Expr -> Expr -> Expr
subst v e = everywhereBut (mkQ False captured) (mkT replace)
      where
      captured (Lam v' _) | v == v' = True
      captured _ = False
      replace (Var v') | v == v' = e
      replace e = e

consts :: Expr -> [Int]
consts = everything (++) (mkQ [] collect)
      where
      collect :: Expr -> [Int]
      collect (Num n) = [n]
      collect _ = []

freeVars :: Expr -> [String]
freeVars = everythingWithContext [] (++) (mkQ ((,) []) collect)
      where
      collect (Var v) bound | v `notElem` bound = ([v], bound)
      collect (Lam b _) bound = ([], b:bound)
      collect _ bound = ([], bound)

-- example = f (\g -> g 0)

example :: Expr
example = App (Var "f") ("g" `Lam` (Var "g" `App` Num 0))
