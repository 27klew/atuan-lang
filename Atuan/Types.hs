{-# LANGUAGE InstanceSigs #-}
module Atuan.Types where


import Data.Set (Set(..), empty, union, unions, singleton)

import Data.Map (Map(..), empty, union, keys, intersection, fromList, toList, lookup)
import Control.Monad.Except (MonadError(throwError), when, unless, lift, foldM)
import Control.Monad ( zipWithM )
-- import Data.Text (intercalate)

data Type = TInt | TFun Type Type | TVar String | ADT String [Type]

type Subst = Map String Type


pars :: String -> String
pars s = "(" ++ s ++ ")"


instance Show Type where
  show :: Type -> String
  show TInt = "Int"
  show (TFun t1 t2) =  pars (show t1) ++ " -> " ++ pars (show t2)
  show (TVar s) = "tvar: " ++ s
  show (ADT name ts) = "ADT: " ++ name ++ show ts



fv :: Type -> Set String
fv TInt = Data.Set.empty
fv (TFun t1 t2) = fv t1 `Data.Set.union` fv t2
fv (TVar s) = singleton s
fv (ADT _ ts) = unions $ map fv ts


type Expected a = Either String a






unionS :: Subst -> Subst -> Expected Subst
unionS s1 s2 = do
    let inter = keys $ intersection s1 s2
    let Just vals1 = sequence $ map (`Data.Map.lookup` s1) inter
    let Just vals2 = sequence $ map (`Data.Map.lookup` s2) inter


    -- TODO... maybe, there should be also unification
    --         (a -> t1), (a -> t2) with something like (a -> least_general(t1, t2))
    --         (now, this is not really done)
    --          this only checks that it is possible to be done

    vals <- sequence $ map (uncurry unif) (zip vals1 vals2)

    s <- foldM unionS Data.Map.empty vals


    return $ s1 `Data.Map.union` s2 `Data.Map.union` s





unif :: Type -> Type -> Expected Subst

unif TInt TInt = return Data.Map.empty

unif (TFun x1 r1) (TFun x2 r2) = do
    s1 <- unif x1 x2
    s2 <- unif r1 r2
    s1 `unionS` s2

unif (TVar s1) (TVar s2) = do
    if s1 == s2 then
        return Data.Map.empty
    else
        return $ fromList [(s1, TVar s2)]

unif (TVar s1) t2 = do
    let vars = fv t2
    when (s1 `elem` vars)
        (throwError $ "Cyclical dependency on variable " ++ s1)
    return $ fromList [(s1, t2)]

unif t2 (TVar s1) = unif (TVar s1) t2

unif (ADT name1 ts1) (ADT name2 ts2) = do
    unless (name1 == name2)
        (throwError $ "Types not compatible: " ++ name1 ++ ", " ++ name2)

    ss <- Control.Monad.zipWithM unif ts1 ts2
    foldM unionS Data.Map.empty ss



unif t1 t2 = throwError $ "Types not compatible: " ++ show t1 ++ ", " ++ show t2




int = TInt
intToInt = TFun int int


s = unif int int




testU :: Type -> Type -> String
testU t1 t2 = case unif t1 t2 of
  Left s -> "Error " ++ s
  Right map -> "Correct " ++ show (toList map)


apply :: Subst -> Type -> Type
apply s (TVar n) = case Data.Map.lookup n s of
  Nothing -> TVar n
  Just ty -> ty
apply s (TFun t1 t2) = TFun (apply s t1)  (apply s t2)
apply s (ADT name ts) = ADT name (map (apply s) ts)
apply _ t = t




-- | Ints are the same.
-- >>> testU int int == "Correct []"
-- True


-- | Functions are the same.
-- >>> testU intToInt intToInt == "Correct []"
-- True


-- | Int is not a function 
-- >>> testU int intToInt
-- "Error Types not compatible: Int, (Int) -> (Int)"




-- | More general value.
-- >>> testU (TVar "a") TInt
-- "Correct [(\"a\",Int)]"

-- | More general function 
-- >>> testU (TFun (TVar "a") (TVar "a")) intToInt
-- "Correct [(\"a\",Int)]"


-- | Cyclic dependence
-- >>> testU (TVar "a") (TFun (TVar "a") (TVar "a"))
-- "Error Cyclical dependency on variable a"


-- | Non empty intersection (trivial)
-- >>> testU (TFun (TVar "a") (TVar "a")) (TFun (TVar "b") (TVar "b"))
-- "Correct [(\"a\",tvar: b)]"


-- | Non empty intersection
-- >>> testU (TFun (TVar "a") (TVar "a")) (TFun (TVar "b") TInt)
-- "Correct [(\"a\",tvar: b),(\"b\",Int)]"

-- | Non empty intersection
-- >>> testU (TFun (TVar "a") (TVar "a")) (TFun TInt (TVar "b") )
-- "Correct [(\"a\",Int),(\"b\",Int)]"

a = TVar "a"
b = TVar "b"

aToA = TFun a a 

bToInt =  TFun b int

cToOther = TFun (TVar "c") (ADT "MyType" [])


-- TODO: this is handled incorrectly (Should result in error, that Int is not MyType)

-- | (a -> a) -> (a -> a )   &&   (b -> Int) -> (c -> MyType)
-- >>> testU (TFun aToA aToA) (TFun bToInt cToOther)
-- "Correct [(\"a\",tvar: b),(\"b\",Int),(\"c\",ADT: MyType[])]"



