{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

data Tm = TmVar String
        | TmAbs String Tm
        | TmApp Tm Tm deriving (Show)

data Ty = TyVar String
        | TyArr Ty Ty
        | TyUnk Unknown deriving (Show)

type Unknown = Int
    
data Constraint = Constraint Unknown Ty

newtype Check a = Check { runCheck :: StateT Int Maybe a } deriving (Functor, Applicative, Monad, MonadState Int)

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

typeOf :: Tm -> Maybe Ty
typeOf tm = flip evalStateT 0 $ runCheck $ do
  (cs, ty) <- collect tm
  sol <- solve cs
  return $ substitute sol ty

type Env = M.Map String Unknown

noType :: Check a
noType = Check (lift Nothing)

fresh :: Check Int
fresh = do
  modify succ
  get

collect :: Tm -> Check ([Constraint], Ty)
collect = collect' M.empty
  where 
  collect' :: Env -> 
              Tm ->
              Check ([Constraint], Ty)
  collect' env (TmVar nm) = 
    case M.lookup nm env of
      Nothing -> noType
      Just u -> return ([], TyUnk u)
  collect' env (TmAbs nm body) = do
    u <- fresh
    (cs, t) <- collect' (M.insert nm u env) body
    return (cs, TyArr (TyUnk u) t)
  collect' env (TmApp tm1 tm2) = do
    (cs1, ty1) <- collect' env tm1
    (cs2, ty2) <- collect' env tm2
    [u1, u2, u3] <- replicateM 3 fresh
    -- ty1 is a function type
    let cs3 = [Constraint u1 (TyArr (TyUnk u2) (TyUnk u3)), Constraint u1 ty1]
    -- ty2 matches ty1
    let cs4 = [Constraint u2 ty2]
    return (join [cs1, cs2, cs3, cs4], TyUnk u3)

bottom :: Solution
bottom = Solution TyUnk

solve :: [Constraint] -> Check Solution
solve = refine bottom
  where
  refine :: Solution -> [Constraint] ->
            Check Solution
  refine s [] = return s
  refine s (c:cs) = do
    let s' = Solution $ replace c . runSolution s
    cs' <- replaceC c cs
    refine s' cs'

replace :: Constraint -> Ty -> Ty
replace (Constraint u t) (TyUnk u1) | u == u1 = t
replace c (TyArr t1 t2) = TyArr (replace c t1) (replace c t2)
replace _ t = t

replaceC :: Constraint -> 
            [Constraint] -> 
            Check [Constraint]
replaceC _ [] = return []
replaceC c@(Constraint u t) (Constraint u1 t1 : cs) 
  | u == u1 = (++) <$> unify t t1 <*> replaceC c cs
  | otherwise = (:) <$> pure (Constraint u1 (replace c t1)) <*> replaceC c cs

unify :: Ty -> Ty -> Check [Constraint]
unify (TyVar v1) (TyVar v2) 
  | v1 == v2 = return []
unify (TyArr t1 t2) (TyArr t3 t4) = 
  (++) <$> unify t1 t3 
       <*> unify t2 t4 
unify (TyUnk u) t = do
  occursCheck u t
  return [Constraint u t]
unify t1 t2@(TyUnk u) = 
  unify t2 t1
unify _ _ = noType

occursCheck :: Unknown -> Ty -> Check ()
occursCheck u (TyUnk u1) 
  | u == u1 = return ()
occursCheck u t = go u t
  where
  go u (TyUnk u1) 
    | u == u1 = noType
  go u (TyArr t1 t2) = 
    const <$> occursCheck u t1 
          <*> occursCheck u t2
  go _ _ = return ()
