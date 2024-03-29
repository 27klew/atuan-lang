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
import Atuan.Abs (BNFC'Position, Constr' (..), Ident (..))
import Control.Monad.Identity (Identity (runIdentity))
import GHC.Debug (debugErrLn)
import qualified Atuan.CollectTypes as CollectTypes (ADTs (..), ADT (..))
import Control.Monad.Trans.Except (Except)
import Data.Maybe (isNothing, catMaybes)
import Maybes (isJust)
-- import Atuan.MatchComplete (checkTotality)

import Atuan.Types


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


class Types a where
    freeVars    ::  a -> Set.Set String
    apply  ::  Subst -> a -> a



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

data TIState = TIState { tiSupply :: Int, adts :: CollectTypes.ADTs Pos}


type TI a = ExceptT String (ReaderT TypeEnv (StateT TIState Identity)) a

runTI :: TI a -> CollectTypes.ADTs Pos -> (Either String a, TIState)
runTI t  adts = do
    -- do (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    --    return (res, st)
    runIdentity (runStateT (runReaderT (runExceptT t) initTIEnv) initTIState)
        where initTIEnv = emptyTypeEnv
              initTIState = TIState{tiSupply = 0, adts = adts}

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

instantiateAnn :: Type -> TI Type
instantiateAnn ty = do
    let vars = freeVars ty
    let s = Scheme (Set.toList vars) ty
    instantiate s



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
unique xs = length (Set.fromList xs) == length xs

checkLabel :: Maybe Type -> Type -> TI Subst
checkLabel Nothing _ = return nullSubst
checkLabel (Just label') ty = do
    label <- instantiateAnn label'
    sub <- mgu label ty
    
    let vars = freeVars label
    let vals' = [Map.lookup k sub | k <- Set.toList vars]
    let vals'' = catMaybes vals'

    -- let vals = Map.elems sub
    unless (all isVar vals'' && unique vals'')
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


ti exp@(EMatch (pos, label) i bs) = do
    (s, t) <- ti (EVar (pos, Nothing) i)
    (s_, t_ ) <- tiMatch bs s t
    res <- checkLabelRes label ( s_,  t_)



    adts' <- get
    let adts_ = adts adts'
    let c = map (checkTotalityMatch adts_) bs
    let c' = unionsCompletion c

    -- trace (show "\n\n\ncompletions: " ++ show c ++ "\nc'" ++ show c' ++ "\n"++ "\n\n\n") $
    c'' <- checkCompletion adts_ c'

    return res


checkTotalityMatch :: CollectTypes.ADTs Pos -> PatternBranch Label -> Completion
checkTotalityMatch adts (PatternBranch pat exp) =
    checkTotality adts pat


checkLabelRes :: Maybe Type -> (Subst, Type) -> TI (Subst, Type)
checkLabelRes label (s, t) = do
    sl <- checkLabel label t
    return (sl `composeSubst` s, apply sl t)


tiMatch :: [PatternBranch Label] -> Subst -> Type -> TI (Subst, Type)
tiMatch [] s t = do
    tv <- newTyVar "a"
    return (s, tv)

tiMatch (b:bs) s t = do
    (sb, tb) <- tiBranch t b

    let s' = sb `composeSubst` s

    (sm, tm) <- tiMatch bs s' (apply s' t)

    smgu <- mgu tm tb
    return (smgu `composeSubst` sm `composeSubst` s', apply smgu tb)


nullTypeEnv = TypeEnv Map.empty


unifTypes :: (Subst, Type) -> (Subst, Type) -> TI (Subst, Type)
unifTypes (s1, t1) (s2, t2) = do
    sm <- mgu t1 t2
    let s1' = sm `composeSubst` s1
    let s2' = sm `composeSubst` s2

    let s3 = s1' `composeSubst` s2'

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
        return (nullSubst, ADT "List" [tv], nullTypeEnv)

  PatternConsList pos pat' pat2 ->  do
    tv <- newTyVar "a"
    let tv' = ADT "List" [tv]


    (s1, t1, tenv1) <- tiPattern pat'
    (s2, t2, tenv2) <- tiPattern pat2

    s2' <- mgu (apply s2 (ADT "List" [t1])) (apply s2 t2)

    s3 <- mgu (apply s2' (ADT "List" [t1])) (apply s2' tv')
    s3' <- mgu (apply s3 t2) (apply s3 tv')

    let inter = Map.intersection (getEnv tenv1) (getEnv tenv2)
    unless (null inter)
        (throwError $ "Multiple declarations of " ++ show inter)

    let s = s3' `composeSubst` s3 `composeSubst` s2' `composeSubst` s2 `composeSubst` s1

    return (s, apply s tv', unionEnv (apply s tenv1) (apply s tenv2))


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

    let tp = foldr TFun tv tys'


    s2 <- mgu tp t'

    return (s2 `composeSubst` s', apply s2 tv , apply s2 ens')


  PatternIdent pos s -> do
    tv <- newTyVar "a"
    let env = TypeEnv $ Map.fromList [(s, Scheme [] tv)]

    return (nullSubst, tv, env)


unionEnvDisjoint :: TypeEnv -> TypeEnv -> TI TypeEnv
unionEnvDisjoint env1 env2 = do
    let inter = intersectEnv env1 env2
    unless (isNullEnv inter)
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
            EVar p_ "double"
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


test' :: Exp Label -> CollectTypes.ADTs Pos -> Either String Type
test' e adts =
    let (res, _) = runTI (typeInference Map.empty e) adts in
    res



test :: Exp Label-> CollectTypes.ADTs Pos -> IO ()
test e adts =
    let  (res, _) = runTI (typeInference Map.empty e) adts in
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


testEnv :: TypeEnv -> CollectTypes.ADTs Pos -> Exp Label -> IO ()
testEnv env adts e =
    let TypeEnv env' = unionEnv defaultEnv env in
    let  (res, _) = runTI (typeInference env' e) adts in
        case res of
          Left err  ->  putStrLn $ show e ++ "\n " ++ err ++ "\n"
          Right t   ->  putStrLn $ show e ++ " :: " ++ show t ++ "\n"

testEnv' :: TypeEnv -> CollectTypes.ADTs Pos -> Exp Label -> IO (Either String Type)
testEnv' env adts e =
    let TypeEnv env' = unionEnv defaultEnv env in
    let  (res, _) = runTI (typeInference env' e) adts in
        return res



testDefault = testEnv defaultEnv


main :: IO ()
main = mapM_ (\x -> test x (CollectTypes.ADTs Map.empty Map.empty Map.empty)) [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19]
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






data Completion = Full | Partial String (Map.Map String [Completion]) deriving (Show, Eq)

unionCompletionList :: [Completion] -> [Completion] -> [Completion]
unionCompletionList c1 c2 =
    if length c1 /= length c2 then error $ "Bad union" ++ show c1 ++ "vs "  ++ show c2
        else
    zipWith unionCompletion c1 c2

unionCompletion :: Completion -> Completion -> Completion
unionCompletion Full _ = Full
unionCompletion _ Full = Full
unionCompletion p1@(Partial s1 m1) p2@(Partial s2 m2) =
    if (s1 /= s2)
        then error $ "Bad union" ++ show p1 ++ "vs "  ++ show p2
    else
        Partial s1 (Map.unionWith unionCompletionList m1 m2)

checkTotality :: CollectTypes.ADTs a -> Pattern b -> Completion
checkTotality adts patt = case patt of
  PatternEmptyList a -> Partial "List" (Map.fromList [("Empty", [])])
  PatternConsList a pat pat' -> (
        let c1 = checkTotality adts pat in
        let c2 = checkTotality adts pat' in
            Partial "List" (Map.fromList [("Cons", [c1, c2])])
        )
  PatternConstr a s pats -> (
        let pats' = map (checkTotality adts) pats in
        -- let i = (case Map.lookup (Ident s) (CollectTypes.from_constr adts) of
        --             Nothing -> error $ "Constructor not found " ++ s ++ "\n"
        --             Just (DataConstructor _ (Ident i) _) -> i
        --         )
        -- in  
        let (Ident i) = getADT adts (Ident s) in

        Partial i (Map.fromList [(s, pats')])
    )
  PatternIdent a s -> Full


checkCompletionMap :: CollectTypes.ADTs a -> (String ,[Completion]) -> TI ()
checkCompletionMap adts (constr, cs) = do
    mapM_ (checkCompletion adts) cs
    `catchError`
    (\err -> throwError $ " (" ++ constr ++ "): " ++ err)
    -- return () -- TODO

checkCompletion :: CollectTypes.ADTs a -> Completion -> TI ()
checkCompletion adts Full = return ()
checkCompletion adts (Partial s m) = do
    mapM_ (checkCompletionMap adts) (Map.toList m)
        `catchError`
            (\err -> throwError $ "In " ++ s  ++ ": " ++ err)
    -- let Just (DataConstructor _ (Ident cons_i) _) = Map.lookup (Ident s) (CollectTypes.from_constr adts) 
    let Just adt = Map.lookup (Ident s) (CollectTypes.from_name adts)
    let csss = map (\(Ident i) -> i) (CollectTypes.constrs adt)

    let all_c = Set.fromList csss
    let present_c = Set.fromList (Map.keys m)
    let missing_c = Set.difference all_c present_c

    unless (null missing_c)
        (throwError $ "unchecked variants: " ++ show missing_c)


getADT :: CollectTypes.ADTs a -> Ident -> Ident
getADT adts cons =
    let Just adt = Map.lookup cons (CollectTypes.from_constr_adt adts) in
        adt


unionsCompletion :: [Completion] -> Completion
unionsCompletion [] = error "Illegal use of unionsCompletion"
unionsCompletion ([c]) = c
unionsCompletion (c:cs) = unionCompletion c (unionsCompletion cs)