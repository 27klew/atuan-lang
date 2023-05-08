{-# LANGUAGE InstanceSigs #-}
-- module Atuan.Types where


import Data.Set (Set(..), empty, union, unions, singleton, fromList, difference, toList, member)

import Data.Map (Map(..), empty, union, keys, intersection, fromList, toList, lookup, delete, map, elems, singleton, insert)
import Control.Monad.Except (MonadError(throwError, catchError), when, unless, lift, foldM)
import Control.Monad ( zipWithM )
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Data.Functor.Identity (Identity)
-- import Data.Text (intercalate)

data Type = TInt | TFun Type Type | TVar String | ADT String [Type] deriving (Eq, Ord)

type Subst = Map String Type

nullSubst :: Subst
nullSubst = Data.Map.empty
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Data.Map.map (apply s1 ) s2 ) `Data.Map.union` s1


newtype TypeEnv = TypeEnv (Data.Map.Map String Scheme)


remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env ) var = TypeEnv (Data.Map.delete var env )

instance Types TypeEnv where
    ftv (TypeEnv env ) = ftv (Data.Map.elems env )
    apply s (TypeEnv env ) = TypeEnv (Data.Map.map (apply s) env )


generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = Data.Set.toList ((ftv t) `Data.Set.difference` (ftv env ))



pars :: String -> String
pars s = "(" ++ s ++ ")"


instance Show Type where
  show :: Type -> String
  show TInt = "Int"
  show (TFun t1 t2) =  pars (show t1) ++ " -> " ++ pars (show t2)
  show (TVar s) = "tvar: " ++ s
  show (ADT name ts) = "ADT: " ++ name ++ show ts




type Expected a = Either String a






unionS :: Subst -> Subst -> Expected Subst
unionS s1 s2 = do
    let inter = keys $ intersection s1 s2
    let Just vals1 = sequence $ Prelude.map (`Data.Map.lookup` s1) inter
    let Just vals2 = sequence $ Prelude.map (`Data.Map.lookup` s2) inter


    -- TODO... maybe, there should be also unification
    --         (a -> t1), (a -> t2) with something like (a -> least_general(t1, t2))
    --         (now, this is not really done)
    --          this only checks that it is possible to be done

    vals <- sequence $ Prelude.map (uncurry unif) (zip vals1 vals2)

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
        return $ Data.Map.fromList [(s1, TVar s2)]

unif (TVar s1) t2 = do
    let vars = ftv t2
    when (s1 `elem` vars)
        (throwError $ "Cyclical dependency on variable " ++ s1)
    return $ Data.Map.fromList [(s1, t2)]

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


data Scheme = Scheme [String ] Type


class Types a where
    ftv :: a -> Set String
    apply :: Subst -> a -> a



testU :: Type -> Type -> String
testU t1 t2 = case unif t1 t2 of
  Left s -> "Error " ++ s
  Right map -> "Correct " ++ show (Data.Map.toList map)


instance Types Type where

    apply :: Subst -> Type -> Type
    apply s (TVar n) = case Data.Map.lookup n s of
        Nothing -> TVar n
        Just ty -> ty
    apply s (TFun t1 t2) = TFun (apply s t1)  (apply s t2)
    apply s (ADT name ts) = ADT name (Prelude.map (apply s) ts)
    apply _ t = t

    ftv :: Type -> Set String
    ftv TInt = Data.Set.empty
    ftv (TFun t1 t2) = ftv t1 `Data.Set.union` ftv t2
    ftv (TVar s) = Data.Set.singleton s
    ftv (ADT _ ts) = unions $ Prelude.map ftv ts


instance Types Scheme where
  ftv :: Scheme -> Set String
  ftv (Scheme vars t) = (ftv t) `difference` (Data.Set.fromList vars)
  apply :: Subst -> Scheme -> Scheme
  apply s (Scheme vars t) = Scheme vars (apply (foldr Data.Map.delete s vars) t)


instance Types a => Types [a ] where
    apply s = Prelude.map (apply s)
    ftv l = foldr Data.Set.union Data.Set.empty (Prelude.map ftv l )












data TIEnv = TIEnv {}

data TIState = TIState{tiSupply :: Int }

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

type TIb a = ReaderT TIEnv (StateT TIState (IO)) a


runTI :: TI a -> IO (Either String a, TIState)
runTI t = do 
    (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv ) initTIState
    return (res, st)
    where 
        initTIEnv = TIEnv
        initTIState = TIState{tiSupply = 0}



-- newTyVarb :: String -> TIb Type
-- newTyVarb prefix = do
--     s <- lift get
--     return (TVar prefix)


-- TODO: why do I need to lift here ?!?!
newTyVar :: String -> TI Type
newTyVar prefix = do 
    s <- lift $ lift get
    (lift . lift . put) s{tiSupply = tiSupply s + 1}
    return (TVar (prefix ++ show (tiSupply s)))



instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do 
    nvars <- mapM (\_ -> newTyVar "a") vars
    let s = Data.Map.fromList (zip vars nvars)
    return $ apply s t


mgu :: Type -> Type -> TI Subst
mgu (TFun l r ) (TFun l' r') = do 
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r ) (apply s1 r')
    return (s1 `composeSubst` s2 )
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt TInt = return nullSubst
mgu (ADT n ts) (ADT n' ts') = throwError $ "TODO: not yet implemented ADT unification"

mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++
        " vs. " ++ show t2


varBind :: String -> Type -> TI Subst
varBind u t 
    | t == TVar u = return nullSubst
    | u `Data.Set.member` ftv t = throwError $ "occurs check fails: " ++ u ++ " vs. " ++ show t
    | otherwise = return (Data.Map.singleton u t)





data Exp = EVar String
    | ELit Lit
    | EApp Exp Exp
    | EAbs String Exp
    | ELet String Exp Exp
    deriving (Eq, Ord, Show)
data Lit = LInt Integer
    | LBool Bool
    deriving (Eq, Ord, Show)






tiLit :: Lit -> TI (Subst, Type)
tiLit (LInt _) = return (nullSubst, TInt)
tiLit (LBool _) = return (nullSubst, (ADT "Bool" []))


ti :: TypeEnv -> Exp -> TI (Subst, Type)
ti (TypeEnv env ) (EVar n) =
    case Data.Map.lookup n env of
        Nothing -> throwError $ "unbound variable: " ++ n
        Just sigma -> do 
            t <- instantiate sigma
            return (nullSubst, t)
ti _ (ELit l ) = tiLit l
ti env (EAbs n e) = do 
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `Data.Map.union` (Data.Map.singleton n (Scheme [ ] tv )))
    (s1 , t1 ) <- ti env'' e
    return (s1 , TFun (apply s1 tv ) t1 )
ti env exp@(EApp e1 e2 ) = do 
    tv <- newTyVar "a"
    (s1 , t1 ) <- ti env e1
    (s2 , t2 ) <- ti (apply s1 env ) e2
    s3 <- mgu (apply s2 t1 ) (TFun t2 tv )
    return (s3 `composeSubst` s2 `composeSubst` s1 , apply s3 tv )
    `catchError`
    \e -> throwError $ e ++ "\n in " ++ show exp

ti env (ELet x e1 e2 ) = do 
    (s1 , t1 ) <- ti env e1
    let TypeEnv env' = remove env x
        t' = generalize (apply s1 env ) t1
        env'' = TypeEnv (Data.Map.insert x t' env')
    (s2 , t2 ) <- ti (apply s1 env'') e2
    return (s1 `composeSubst` s2 , t2 )


typeInference :: Data.Map.Map String Scheme -> Exp -> TI Type
typeInference env e = do 
    (s, t) <- ti (TypeEnv env ) e
    return (apply s t)


e0 = ELet "id" (EAbs "x" (EVar "x")) (EVar "id")
e1 = ELet "id" (EAbs "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
e2 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EVar "id") (EVar "id"))
e3 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EApp (EVar "id") (EVar "id")) (ELit (LInt 2)))
e4 = ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x"))) (EVar "id")
e5 = EAbs "m" (ELet "y" (EVar "m") (ELet "x" (EApp (EVar "y") (ELit (LBool True))) (EVar "x")))
e6 = EApp (ELit (LInt 2)) (ELit (LInt 2))


test :: Exp -> IO ()
test e = do 
    (res, _) <- runTI (typeInference Data.Map.empty e)
    case res of
        Left err -> putStrLn $ show e ++ "\n " ++ err ++ "\n"
        Right t -> putStrLn $ show e ++ " :: " ++ show t ++ "\n"


-- e7 = ELet "f" ()


main :: IO ()
main = mapM_ test [e0, e1, e2, e3, e4, e5, e6]



-- >>> mapM_ test [e0, e1, e2, e3, e4, e5, e6]




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



