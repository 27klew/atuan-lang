-- Based on: https://github.com/mgrabmueller/AlgorithmW
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Atuan.AlgorithmW where


import qualified Data.Map as Map
import qualified Data.Set as Set


import Control.Monad.Except(runExceptT, MonadError(throwError, catchError), ExceptT, when)
import Control.Monad.Reader(ReaderT(runReaderT), MonadReader (ask, local) )
import Control.Monad.State(MonadState(put, get), StateT(runStateT) )
import Control.Monad (foldM, unless)


import qualified Text.PrettyPrint as PP
import Debug.Trace
import Atuan.Abs (BNFC'Position)
import Control.Monad.Identity (Identity (runIdentity))
import GHC.Debug (debugErrLn)


type Pos = BNFC'Position

type Label = (Pos, Maybe Type)

data Exp a =  EVar a String
             |  ELit a (Lit a)
             |  EApp a (Exp a) (Exp a)
             |  EAbs a String (Exp a)
             |  ELet a String (Exp a) (Exp a)
             |  ELetRec a String (Exp a) (Exp a)
             |  EIf a (Exp a) (Exp a) (Exp a)
             |  EBinOp a (Exp a) OpBin (Exp a)
             |  EUnOp  a OpUn (Exp a)
             |  EMatch a String [PatternBranch a]

             deriving (Eq, Ord)



data PatternBranch a = PatternBranch (Pattern a) (Exp a) deriving (Eq, Ord)

data Pattern a =
    PatternEmptyList a
    | PatternConsList a (Pattern a) (Pattern a)
    -- | PatternLiteral Exp
    |  PatternConstr a String [Pattern a]
    |  PatternIdent a String
    deriving (Eq, Ord, Show)



data OpUn = OpNeg | OpNot deriving (Eq, Ord)
data OpBin = OpMul MulOp | OpAdd AddOp | OpRel RelOp | OpAnd | OpOr deriving (Eq, Ord)

data MulOp = Times | Div | Mod deriving (Eq, Ord)

data AddOp = Plus  | Minus deriving (Eq, Ord)

data RelOp = LTH  | LE  | GTH  | GE  | EQU | NE deriving (Eq, Ord)


opBinRes :: OpBin -> Type
opBinRes (OpMul _) = TInt
opBinRes (OpAdd _) = TInt
opBinRes (OpRel _) = TBool
opBinRes OpOr = TBool
opBinRes OpAnd = TBool


opBinArg :: OpBin -> Type
opBinArg (OpMul _) = TInt
opBinArg (OpAdd _) = TInt
opBinArg (OpRel _) = TInt
opBinArg OpOr = TBool
opBinArg OpAnd = TBool


opUnRes :: OpUn -> Type
opUnRes OpNeg = TInt
opUnRes OpNot = TBool


opUnArg :: OpUn -> Type
opUnArg OpNeg = TInt
opUnArg OpNot = TBool

data Lit a    =  LInt a Integer
             |  LBool a Bool
             |  LList a [Exp a]
             deriving (Eq, Ord)

data Type    =  TVar String
             |  TInt
             |  TBool
             |  TFun Type Type
             |  ADT String [Type]
             deriving (Eq, Ord)

data Scheme  =  Scheme [String] Type


class Types a where
    freeVars    ::  a -> Set.Set String
    apply  ::  Subst -> a -> a


-- class Collectible a where
--     collect :: (a -> Set.Set b) -> a -> Set.Set b 



-- instance Collectible Type where
--   collect :: (Type -> Set.Set b) -> Type -> Set.Set b
--   collect f t = case t of
--     TVar s -> f (TVar s)
--     TInt -> f (TInt s)
--     TBool -> _
--     TFun ty ty' -> _
--     ADT s tys -> _


instance Types Type where
    freeVars (TVar n)      =  Set.singleton n
    freeVars TInt          =  Set.empty
    freeVars TBool         =  Set.empty
    freeVars (TFun t1 t2)  =  freeVars t1 `Set.union` freeVars t2
    freeVars (ADT _ ts)    =  foldr (Set.union . freeVars) Set.empty ts


    apply s (TVar n)      =  case Map.lookup n s of
                               Nothing  -> TVar n
                               Just t   -> t

    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)

    apply s TInt             =  TInt
    apply s TBool            =  TBool
    apply s (ADT name ts)    =  ADT name (map (apply s) ts)


instance Types Scheme where
    freeVars (Scheme vars t)      =  freeVars t `Set.difference` Set.fromList vars

    apply s (Scheme vars t)  =  Scheme vars (apply (foldr Map.delete s vars) t)



instance Types a => Types [a] where
    apply s  =  map (apply s)
    freeVars l    =  foldr (Set.union . freeVars) Set.empty l



type Subst = Map.Map String Type

nullSubst  ::  Subst
nullSubst  =   Map.empty

composeSubst         :: Subst -> Subst -> Subst
composeSubst s1 s2   = Map.map (apply s1) s2 `Map.union` s1



remove                    ::  TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var  =  TypeEnv (Map.delete var env)

instance Types TypeEnv where
    freeVars (TypeEnv env)      =  freeVars (Map.elems env)
    apply s (TypeEnv env)  =  TypeEnv (Map.map (apply s) env)



generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList (freeVars t `Set.difference` freeVars env)


newtype TypeEnv = TypeEnv {getEnv :: Map.Map String Scheme} deriving (Show)

newtype TIState = TIState { tiSupply :: Int }


type TI a = ExceptT String (ReaderT TypeEnv (StateT TIState Identity)) a

runTI :: TI a -> (Either String a, TIState)
runTI t = do
    -- do (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    --    return (res, st)
    runIdentity (runStateT (runReaderT (runExceptT t) initTIEnv) initTIState)
        where initTIEnv = emptyTypeEnv
              initTIState = TIState{tiSupply = 0}

newTyVar :: String -> TI Type
newTyVar prefix =
    do  s <- get
        put s{tiSupply = tiSupply s + 1}
        return (TVar  (prefix ++ show (tiSupply s)))




instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do  nvars <- mapM (\ _ -> newTyVar "a") vars
                                  let s = Map.fromList (zip vars nvars)
                                  return $ apply s t

mguList :: [Type] -> [Type] -> TI Subst
mguList [] [] = return nullSubst
mguList (t1:ts1) (t2:ts2) = do
    s <- mgu t1 t2
    sl <- mguList (map (apply s) ts1) (map (apply s) ts2)

    return $ sl `composeSubst` s

mguList _ _ = throwError "Different number of type arguments."


mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r')  =  do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s1 `composeSubst` s2)
mgu (TVar u) t =  varBind u t
mgu t (TVar u) =  varBind u t
mgu TInt TInt  =  return nullSubst
mgu TBool TBool =  return nullSubst
mgu (ADT name1 ts1) (ADT name2 ts2) = do
        unless (name1 == name2)
            (throwError $ "Types " ++ name1 ++ " and " ++ name2 ++ "cannot be unified.")
        unless (length ts1 == length ts2)
            (throwError $ "ADTS: " ++ name1 ++ "and " ++ name2 ++ " have different number of arguments" )

        mguList ts1 ts2


mgu t1 t2                    =  throwError $ "types do not unify: " ++ show t1 ++
                                " vs. " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t  | t == TVar u           =  return nullSubst
             | u `Set.member` freeVars t  =  throwError $ "occurs check fails: " ++ u ++
                                         " vs. " ++ show t
             | otherwise             =  return (Map.singleton u t)





tiLit :: Lit Label -> TI (Subst, Type)
tiLit (LInt _ _)   =  return (nullSubst, TInt)
tiLit (LBool _ _)  =  return (nullSubst, TBool)
tiLit (LList _ []) =  do
    tv <- newTyVar "a"
    return (nullSubst, ADT "List" [tv])

tiLit (LList pos (x:xs)) = do
    (s, t) <- ti x
    (sl, tl) <- local (apply s) (tiLit  (LList pos xs))
    s3 <- mgu tl (ADT "List" [apply sl t])

    -- catchError


    return (s3 `composeSubst` sl `composeSubst` s, apply s3 tl)


instance Types a => Types (Maybe a) where
  freeVars :: Types a => Maybe a -> Set.Set String
  freeVars ot = maybe Set.empty freeVars ot
  apply :: Types a => Subst -> Maybe a -> Maybe a
  apply sub ot = case ot of
    Nothing -> Nothing
    Just a -> Just $ apply sub a

instantiateAnn :: Type -> TI (Type)
instantiateAnn ty = do
    let vars = freeVars ty
    let s = Scheme (Set.toList vars) ty 
    ty' <- instantiate s 
    return ty'



mguAnn :: Maybe Type -> Type -> TI Subst
mguAnn Nothing _ = return nullSubst
mguAnn (Just t1) t2 = mgu t1 t2


-- mguAnn' :: Maybe Type -> Type -> TI Subst 
-- mguAnn' Nothin _ = return nullSubst
-- 

isVar :: Type -> Bool
isVar (TVar _ ) = True
isVar _ = False

unique :: Ord a => [a] -> Bool
unique xs = (length (Set.fromList xs) == length xs)

checkLabel :: Maybe Type -> Type -> TI Subst
checkLabel Nothing _ = return nullSubst
checkLabel (Just label') ty = do 
    label <- instantiateAnn label'
    sub <- mgu label ty
    let vals = Map.elems sub
    unless (all isVar vals && unique vals)
        (throwError $ "Annotation is too general " ++ show label' ++ " vs " ++ show ty)
    return sub


ti  :: Exp Label -> TI (Subst, Type)
ti (EVar (pos, label) n) = do
    env <- ask
    case Map.lookup n (getEnv env) of
       Nothing     ->  throwError $ "unbound variable: " ++ n
       Just sigma  ->  do  t <- instantiate sigma
                           checkLabelRes label (nullSubst, t)
ti (ELit (pos, label) l) = do 
    res <- tiLit l
    checkLabelRes label res
ti (EAbs (pos, label) n e) = do
        tv <- newTyVar "a"
        env <- ask
        let TypeEnv env' = remove env n
            env'' = TypeEnv (env' `Map.union` Map.singleton n (Scheme [] tv))
        (s1, t1) <- local (const env'') (ti e)
        let abs_t = apply s1 $ TFun tv t1

        checkLabelRes label (s1, apply s1 abs_t)

ti exp@(EApp (pos, label) e1 e2) = do
        tv <- newTyVar "a"
        (s1, t1) <- ti e1
        (s2, t2) <- local (apply s1) (ti e2)
        s3 <- mgu (apply s2 t1) (TFun t2 tv)
        checkLabelRes label (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
    `catchError`
    \e -> throwError $ e ++ "\n in " ++ show exp

ti (ELet (pos, label) x e1 e2) = do
        (s1, t1) <- ti e1
        env <- ask
        let TypeEnv env' = remove env x
            t' = generalize (apply s1 env) t1
            env'' = TypeEnv (Map.insert x t' env')
        (s2, t2) <- local (const $ apply s1 env'') (ti  e2)
        checkLabelRes label (s1 `composeSubst` s2, t2)


ti (ELetRec (pos, label) x e1 e2) = do
    env <- ask
    let TypeEnv env' = remove env x
    tv <- newTyVar "a"
    let tv' = Scheme [] tv
    (s1, t1) <- local (const $ TypeEnv (Map.insert x tv' env')) (ti  e1)

    -- (s1, t1) <- ti env e1
    let t' = generalize (apply s1 (TypeEnv env')) t1
    let env'' = TypeEnv (Map.insert x t' env')
    (s2, t2) <- local (const (apply s1 env'')) (ti  e2)
    checkLabelRes label (s1 `composeSubst` s2, t2)


ti (EIf (pos, label) cond e1 e2) = do
    (sc, tc) <- ti cond
    sc' <- mgu (apply sc tc) TBool

    (s1, t1) <- local (apply sc') (ti e1)
    (s2, t2) <- local (apply s1) (ti e2)
    s3 <- mgu (apply s2 t1) (apply s2 t2)

    checkLabelRes label 
        (s3 `composeSubst` s2 `composeSubst` s1 `composeSubst` sc', apply s3 t2)



ti (EBinOp (pos, label) e1 op e2) = do
    let ta = opBinArg op
    (s1, t1) <- ti e1
    (s2, t2) <- local (apply s1) (ti e2)
    s3 <- mgu (apply s2 t1) (apply s2 t2)
    s4 <- mgu (apply s3 t1) ta

    let tr = opBinRes op

    checkLabelRes label 
        (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, tr)



ti (EUnOp (pos, label) op e) = do
    let targ = opUnArg op
    (s1, t1) <- ti e
    s2 <- mgu (apply s1 t1) targ
    let tres = opUnRes op
    checkLabelRes label (s2 `composeSubst` s1, apply s2 tres)


ti (EMatch (pos, label) i bs) = do
    (s, t) <- ti (EVar (pos, Nothing) i)
    (s_, t_ ) <- tiMatch bs s t
    checkLabelRes label ( s_,  t_)


checkLabelRes :: Maybe Type -> (Subst, Type) -> TI (Subst, Type)
checkLabelRes label (s, t) = do
    sl <- checkLabel label t
    return (sl `composeSubst` s, apply sl t) 


tiMatch :: [PatternBranch Label] -> Subst -> Type -> TI (Subst, Type)
tiMatch [] s t = do
    tv <- newTyVar "a"
    return (s, tv)

tiMatch (b:bs) s t = do
    (sb, tb) <- trace ("\ntiMatch \nbs lenght: " ++ show (length bs)
                        ++ " \nt: " ++show t ++ "\ns: " ++ show s
                        ++ "\n\n"
                        ) (tiBranch t b)

    let s' = trace ("\n\n" ++ "\nsb: " ++ show sb
                        ++ "\ntb: " ++ show tb ++ "\n")
                (sb `composeSubst` s)

    (sm, tm) <- tiMatch bs s' (apply s' t)

    trace (
                -- "\ntiMatch "
                -- ++ "\nbs lenght: " ++ show (length bs) 
                -- ++ " \nt: " ++show t ++ "\ns: " ++ show s ++ "\nsb: " ++ show sb
                ""
                -- ++ "\ntb: " ++ show tb ++ "\ns': "++ show s' 
                ++ "\nsm: " ++ show sm
                ++ "\ntm: " ++ show tm  ++ "\n"
                )
        (do
            smgu <- mgu tm tb
            return (smgu `composeSubst` sm `composeSubst` s', apply smgu tb)
        )


nullTypeEnv = TypeEnv Map.empty


unifTypes :: (Subst, Type) -> (Subst, Type) -> TI (Subst, Type)
unifTypes (s1, t1) (s2, t2) = do
    sm <- mgu t1 t2
    let s1' = sm `composeSubst` s1
    let s2' = sm `composeSubst` s2

    let s3 = s1' `composeSubst` s2'
    -- s3' <- mgu (apply s3 t1) (apply s3 t2)

    -- let s = s3' `composeSubst` s3

    -- trace ("Unif types:" ++ "\ns3' " ++ show s3' ++ "\nt1: "++show t1 ++ "\nt2" ++ show t2 ++ "\n\n")
    --     return (s, apply s t1)



    trace ("\n\nUnif types:\n s1: " ++ show s1 ++ "\nst1: " ++ show t1 ++ "\ns2: " ++show s2 ++ "\nt2" ++ show t2
            ++ "\nsm: " ++ show sm ++ "\ns1': " ++ show s1' ++ "\ns2': " ++ show s2' ++ "\ns3: " ++ show s3 ++"\n\n"
            )
        return (s3, apply s3 t1)


intersectEnv :: TypeEnv -> TypeEnv -> TypeEnv
intersectEnv (TypeEnv t1) (TypeEnv t2) = TypeEnv $ Map.intersection t1 t2


isNullEnv :: TypeEnv -> Bool
isNullEnv (TypeEnv e) = null e


-- unifTypeEnvs :: (Subst, Type, TypeEnv) -> (Subst, Type, TypeEnv) -> TI (Subst, Type, TypeEnv)
-- unifTypeEnvs (s1, t1, e1) (s2, t2, e2) = do
--         (s, t) <- unifTypes (s1, t1) (s2, t2)

--         unless (isNullEnv $ intersectEnv e1 e2)
--             (throwError $ "Multiple definitions of names: " ++ show (intersectEnv e1 e2))

--         let e = unionEnv (apply s e1) (apply s e2)

--         return (s, t, e)





tiPattern :: Pattern Label ->  TI (Subst, Type, TypeEnv)
tiPattern pat = case pat of
  PatternEmptyList pos -> do
        tv <- newTyVar "a"
        return (nullSubst, (ADT "List" [tv]), nullTypeEnv)

  PatternConsList pos pat' pat2 ->  do
    tv <- newTyVar "a"
    let tv' = ADT "List" [tv]


    (s1, t1, tenv1) <- tiPattern pat'
    (s2, t2, tenv2) <- tiPattern pat2

    s2' <- mgu (apply s2 (ADT "List" [t1])) (apply s2 t2)

    s3 <- mgu (apply s2' (ADT "List" [t1])) (apply s2' tv')
    s3' <- mgu (apply s3 t2) (apply s3 tv')

    unless (null $ Map.intersection (getEnv tenv1) (getEnv tenv2))
        (throwError $ "Multiple declarations of " )

    -- throwError $ "PatternCons List tenv :" ++ (show tenv2)

    let s = s3' `composeSubst` s3 `composeSubst` s2' `composeSubst` s2 `composeSubst` s1

    return (s, apply s tv', unionEnv (apply s tenv1) (apply s tenv2))

    -- s3 <- mgu (apply s2 (ADT "List" [t2])) (apply s2 t1)

    -- return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 (ADT "List" [t2]))

  PatternConstr pos con pats -> do
    (s, t) <- ti (EVar pos con)

    ts <- mapM tiPattern pats

    let (ss, tys, ens) = unzip3 ts

    let ss' = foldr composeSubst nullSubst ss

    let ens'' = apply ss' ens

    ens' <- foldM unionEnvDisjoint emptyTypeEnv ens''

    let tys' = apply ss' tys

    let s' = s `composeSubst` ss'

    let t' = apply s' t


    tv <- newTyVar "a"

    let tp = foldr (TFun) tv tys'


    s2 <- mgu tp t'

    return (s2 `composeSubst` s', apply s2 tv , apply s2 ens')


  PatternIdent pos s -> do
    tv <- newTyVar "a"
    let env = TypeEnv $ Map.fromList [(s, Scheme [] tv)]

    return (nullSubst, tv, env)


unionEnvDisjoint :: TypeEnv -> TypeEnv -> TI TypeEnv
unionEnvDisjoint env1 env2 = do
    let inter = intersectEnv env1 env2
    unless (isNullEnv $ inter)
        (throwError $ "Duplicated declarations" ++ show inter)
    return $ unionEnv env1 env2


emptyTypeEnv = TypeEnv Map.empty

tiBranch :: Type -> PatternBranch Label -> TI (Subst, Type)
tiBranch tvar (PatternBranch pat exp)  = do
    (s, t, tenv) <- tiPattern pat


    s' <- mgu (apply s t) (apply s tvar)

    let tenv' = apply s' tenv


    env <- ask
    let env' = unionEnv tenv' env

    -- throwError $ 
    --     "Branch exp \ntenv' "++ show tenv' ++ "\ntvar was: " ++ show tvar ++ "\nt is: " ++ show t
    --      ++ "\nsubst: " ++ show s
    --      ++ "\n tenv: " ++ show tenv 


    (s1, t1) <- local (const $ apply s' env') (ti exp)
    return (s1 `composeSubst` s' `composeSubst` s, t1)



typeInference :: Map.Map String Scheme -> Exp Label -> TI Type
typeInference env e = do
        (s, t) <- local (const $ TypeEnv env) (ti e)
        return (apply s t)



p_ :: Label
p_ = (Just (1, 1), Nothing)

e0  =  ELet p_ "id" (EAbs p_ "x" (EVar p_  "x"))
        (EVar p_ "id")

e1  =  ELet p_ "id" (EAbs p_ "x" (EVar p_ "x"))
        (EApp p_ (EVar p_  "id") (EVar p_ "id"))

e2  =  ELet p_ "id" (EAbs p_ "x" (ELet p_ "y" (EVar p_ "x") (EVar p_ "y")))
        (EApp p_ (EVar p_ "id") (EVar p_ "id"))

e3  =  ELet p_ "id" (EAbs p_ "x" (ELet p_ "y" (EVar p_ "x") (EVar p_ "y")))
        (EApp p_ (EApp p_  (EVar p_ "id") (EVar  p_ "id")) (ELit p_ (LInt p_ 2)))

e4  =  ELet p_ "id" (EAbs p_ "x" (EApp p_ (EVar p_ "x") (EVar p_ "x")))
        (EVar p_ "id")

e5  =  EAbs  p_ "m" (ELet p_ "y" (EVar p_ "m")
                 (ELet p_ "x" (EApp p_ (EVar p_ "y") (ELit p_ (LBool p_  True)))
                       (EVar p_ "x")))

e6  =  EApp p_ (ELit p_ (LInt p_ 2)) (ELit p_ (LInt p_ 2))




e7  = EIf p_ (ELit p_ $ LBool p_ True) (ELit p_ $ LInt p_  4) (ELit p_ $ LInt p_ 5)

e8  = EIf p_ (ELit p_  $ LBool p_  True) (ELit p_ $ LBool p_ True) (ELit p_ $ LInt p_ 5)

e9  = EIf p_ (ELit p_ $ LInt p_  5) (ELit p_ $ LBool p_ True) (ELit p_ $ LInt p_ 5)




e10  = EIf p_ (EBinOp p_  (ELit p_ $ LBool p_ True) OpOr (ELit p_ $ LBool p_ True)) (ELit p_ $ LInt p_  4) (ELit p_  $ LInt p_ 5)

e11  = EIf p_ (EBinOp p_ (ELit p_  $ LInt p_  4) OpOr (ELit p_ $ LBool p_ True)) (ELit p_ $ LBool p_  True) (ELit p_ $ LInt p_ 5)

e12 = EIf p_ (ELit p_ $ LInt p_  5) (ELit p_ $ LBool p_ True) (ELit p_ $ LInt p_ 5)


e13 = ELet p_ "fun" (EAbs p_ "x"
        (
            EIf p_ (ELit p_ $ LBool p_ True)
                (EApp p_ (EVar p_ "fun") (ELit p_ $ LInt p_ 5))
                (ELit p_ $ LInt p_ 5)
        )
    )
    (
        EApp p_ (EVar p_ "fun") (ELit p_ $ LInt p_ 5)
    )

e14 = ELetRec p_ "fun" (EAbs p_ "x"
        (
            EIf p_ (ELit p_ $ LBool p_ True)
                (EApp p_ (EVar p_ "fun") (ELit p_ $ LInt p_ 5))
                (ELit p_ $ LInt p_ 5)
        )
    )
    (
        EApp p_ (EVar p_ "fun") (ELit p_ $ LInt p_ 5)
    )

e15 = ELetRec p_ "fun" (EAbs p_ "x"
        (
            EIf p_ (ELit p_ $ LBool p_ True)
                (EApp p_ (EVar p_ "fun") (EVar p_ "x"))
                (EVar p_ "x")
        )
    )
    (
        EApp p_ (EVar p_ "fun") (ELit p_ $ LInt p_ 5)
    )


e16 = ELetRec p_ "fun" (EAbs p_ "x"
        (
            EIf p_ (ELit p_ $ LBool p_ True)
                (EApp p_ (EVar p_ "fun") (EVar p_ "x"))
                (EVar p_ "x")
        )
    )
    (
        EVar p_ "fun"
    )

e17 = ELetRec p_ "fun" (EAbs p_ "x"
        (
            EIf p_ (ELit p_ $ LBool p_ True)
                (EApp p_ (EVar p_ "fun") (ELit p_ $ LBool p_ True))
                (EVar p_ "x")
        )
    )
    (
        EApp p_ (EVar p_ "fun") (ELit p_ $ LInt p_ 5)
    )


e18 = ELetRec p_ "double" (EAbs p_ "f"
        (
            EAbs p_ "x"
            (
                EApp p_
                    (EVar p_ "f")
                    (EApp p_
                        (EVar p_ "f")
                        (EVar p_ "x")
                    )
            )
        )
    )
    (
        -- EApp 
            (
                (EVar p_ "double")
                -- EApp (EVar "double")
                -- (
                --     EAbs "c"
                --     (
                --         EVar "c"
                --     )
                -- )
            )


            -- (ELit $ LInt 5)
    )


e19 = ELetRec p_ "iter"
    (EAbs p_ "f" (
        EAbs p_ "x" (
            EAbs p_ "n" (
                EIf p_ (EVar p_ "n")
                (EApp p_ (EVar p_ "f") (EVar p_ "x"))
                (EApp p_ (EApp p_ (EVar p_ "iter") (EVar p_ "f")) (EVar p_ "n"))
            )
        )
    ))
    (
        EVar p_ "iter"
    )


test' :: Exp Label -> Either String Type
test' e =
    let (res, _) = runTI (typeInference Map.empty e) in
    res



test :: Exp Label -> IO ()
test e =
    let  (res, _) = runTI (typeInference Map.empty e) in
        case res of
          Left err  ->  putStrLn $ show e ++ "\n " ++ err ++ "\n"
          Right t   ->  putStrLn $ show e ++ " :: " ++ show t ++ "\n"



defaultEnv :: TypeEnv
defaultEnv = TypeEnv $ Map.fromList
        [
            (
            "Cons",
            Scheme ["__l"] (TFun (TVar "__l") (TFun (ADT "List" [TVar "__l"]) (ADT "List" [TVar "__l"])) )
            )
        ]


unionEnv :: TypeEnv -> TypeEnv -> TypeEnv
unionEnv (TypeEnv e1) (TypeEnv e2) = TypeEnv (Map.union e1 e2)


testEnv :: TypeEnv -> Exp Label -> IO ()
testEnv env e =
    let TypeEnv env' = unionEnv defaultEnv env in
    let  (res, _) = runTI (typeInference env' e) in
        case res of
          Left err  ->  putStrLn $ show e ++ "\n " ++ err ++ "\n"
          Right t   ->  putStrLn $ show e ++ " :: " ++ show t ++ "\n"

testEnv' :: TypeEnv -> Exp Label -> IO (Either String Type)
testEnv' env e =
    let TypeEnv env' = unionEnv defaultEnv env in
    let  (res, _) = runTI (typeInference env' e) in
        return res



testDefault = testEnv defaultEnv


main :: IO ()
main = mapM_ test [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19]
-- |Collecting Constraints|
-- |main = mapM_ test' [e0, e1, e2, e3, e4, e5]|



instance Show Type where
    showsPrec _ x = shows (prType x)

prType             ::  Type -> PP.Doc
prType (TVar n)    =   PP.text n
prType TInt        =   PP.text "Int"
prType TBool       =   PP.text "Bool"
prType (TFun t s)  =   prParenType t PP.<+> PP.text "->" PP.<+> prType s
prType (ADT name ts) = PP.text (name ++ ": ") PP.<+> PP.text (show ( map (show . prType) ts))

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      TFun _ _  -> PP.parens (prType t)
                      _         -> prType t

instance Show (Exp a) where
    showsPrec _ x = shows (prExp x)

prExp                  ::  Exp a -> PP.Doc
prExp (EVar _ name)      =   PP.text name
prExp (ELit _ lit)       =   prLit lit
prExp (ELet _ x b body)  =   PP.text "let" PP.<+>
                           PP.text x PP.<+> PP.text "=" PP.<+>
                           prExp b PP.<+> PP.text "in" PP.$$
                           PP.nest 2 (prExp body)


prExp (ELetRec _ x b body)  =   PP.text "letrec" PP.<+>
                           PP.text x PP.<+> PP.text "=" PP.<+>
                           prExp b PP.<+> PP.text "in" PP.$$
                           PP.nest 2 (prExp body)


prExp (EApp _ e1 e2)     =   prExp e1 PP.<+> prParenExp e2
prExp (EAbs _ n e)       =   PP.char '\\' PP.<> PP.text n PP.<+>
                           PP.text "->" PP.<+>
                           prExp e

-- TODO: this is ugly...
prExp (EIf _ c e1 e2)    =   PP.text "IF " PP.<+>  PP.text"(" PP.<+> prExp c PP.<+>  PP.text")  then"
                                        PP.<+>  PP.text"(" PP.<+> prExp e1 PP.<+>  PP.text")  else"
                                        PP.<+>  PP.text"(" PP.<+> prExp e2 PP.<+>  PP.text")  endif"

prExp (EBinOp _ e1 op e2) =  PP.text"(" PP.<+> prExp e1 PP.<+>  PP.text")"
                        PP.<+>  prOpBin op
                        PP.<+>  PP.text"(" PP.<+> prExp e2 PP.<+>  PP.text")"


prExp (EUnOp _ op e) = prOpUn op  PP.<+>  PP.text"(" PP.<+> prExp e PP.<+>  PP.text")"

prExp (EMatch _ i pbs) = PP.text ("Match " ++ i ++ " with ") PP.<+> PP.vcat (map prBranch pbs)

prOpUn OpNeg = PP.text "-"
prOpUn OpNot = PP.text "~"


prPat :: Pattern a -> PP.Doc
prPat pat = case pat of
  PatternEmptyList pos -> PP.text "[]"
  PatternConsList pos pat' pat2 -> prPat pat' PP.<+> PP.text ":" PP.<+> prPat pat2
  PatternConstr pos s pats -> PP.text ("(" ++ s)  PP.<+> PP.hcat (map ((PP.<+> PP.text " ") . prPat) pats) PP.<+> PP.text ")"
  PatternIdent pos s -> PP.text s


prBranch :: PatternBranch a -> PP.Doc
prBranch branch = case branch of
    PatternBranch pat exp -> prPat pat PP.<+> PP.text "->" PP.<+> prExp exp


prOpBin (OpAdd _)= PP.text "+"
prOpBin (OpMul _)= PP.text "*"
prOpBin (OpRel _)= PP.text "<"
prOpBin OpAnd = PP.text "&&"
prOpBin OpOr = PP.text "||"



prParenExp    ::  Exp a -> PP.Doc
prParenExp t  =   case t of
                    ELet _ _ _ _  -> PP.parens (prExp t)
                    EApp _ _ _    -> PP.parens (prExp t)
                    EAbs _ _ _    -> PP.parens (prExp t)
                    _           -> prExp t

instance Show (Lit a) where
    showsPrec _ x = shows (prLit x)

prLit            ::  (Lit a) -> PP.Doc
prLit (LInt _ i)   =   PP.integer i
prLit (LBool _ b)  =   if b then PP.text "True" else PP.text "False"
prLit (LList _ xs) =   PP.hcat (map prExp xs)

instance Show Scheme where
    showsPrec _ x = shows (prScheme x)

prScheme                  ::  Scheme -> PP.Doc
prScheme (Scheme vars t)  =   PP.text "All" PP.<+>
                              PP.hcat
                                (PP.punctuate PP.comma (map PP.text vars))
                              PP.<> PP.text "." PP.<+> prType t




