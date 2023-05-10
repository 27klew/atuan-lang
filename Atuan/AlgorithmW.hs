module Atuan.AlgorithmW where


import qualified Data.Map as Map
import qualified Data.Set as Set




import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State



import qualified Text.PrettyPrint as PP


data Def = Definition String String Exp

newtype Program = Program [Def]

data Exp     =  EVar String
             |  ELit Lit
             |  EApp Exp Exp
             |  EAbs String Exp
             |  ELet String Exp Exp
             |  ELetRec String Exp Exp
             |  EIf  Exp Exp Exp
             |  EBinOp Exp OpBin Exp
             |  EUnOp  OpUn Exp

             deriving (Eq, Ord)


            --  TODO
            --  ELitList a [Val' a] 
            --  EMatch a Ident [PatternBranch' a]

data OpUn = OpNeg | OpNot deriving (Eq, Ord)
data OpBin = OpMul MulOp | OpAdd AddOp | OpRel RelOp | OpAnd | OpOr deriving (Eq, Ord)

data MulOp = Times | Div | Mod deriving (Eq, Ord)

data AddOp = Plus  | Minus deriving (Eq, Ord)

data RelOp = LTH  | LE  | GTH  | GE  | EQU | NE deriving (Eq, Ord) 


tiOpBin :: OpBin -> Type
tiOpBin (OpMul _) = TInt
tiOpBin (OpAdd _) = TInt
tiOpBin (OpRel _) = TBool
tiOpBin OpOr = TBool
tiOpBin OpAnd = TBool

tiOpUn :: OpUn -> Type
tiOpUn OpNeg = TInt
tiOpUn OpNot = TBool


data Lit     =  LInt Integer
             |  LBool Bool
             |  LList [Exp]
             deriving (Eq, Ord)

data Type    =  TVar String
             |  TInt
             |  TBool
             |  TFun Type Type
             |  ADT String [Type]
             deriving (Eq, Ord)

data Scheme  =  Scheme [String] Type




class Types a where
    ftv    ::  a -> Set.Set String
    apply  ::  Subst -> a -> a





instance Types Type where
    ftv (TVar n)      =  Set.singleton n
    ftv TInt          =  Set.empty
    ftv TBool         =  Set.empty
    ftv (TFun t1 t2)  =  ftv t1 `Set.union` ftv t2
    ftv (ADT _ ts)    =  foldr (Set.union . ftv) Set.empty ts

    apply s (TVar n)      =  case Map.lookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s t             =  t



instance Types Scheme where
    ftv (Scheme vars t)      =  ftv t `Set.difference` Set.fromList vars

    apply s (Scheme vars t)  =  Scheme vars (apply (foldr Map.delete s vars) t)



instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv l    =  foldr (Set.union . ftv) Set.empty l



type Subst = Map.Map String Type

nullSubst  ::  Subst
nullSubst  =   Map.empty

composeSubst         :: Subst -> Subst -> Subst
composeSubst s1 s2   = Map.map (apply s1) s2 `Map.union` s1




newtype TypeEnv = TypeEnv (Map.Map String Scheme)




remove                    ::  TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var  =  TypeEnv (Map.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env)      =  ftv (Map.elems env)
    apply s (TypeEnv env)  =  TypeEnv (Map.map (apply s) env)



generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList (ftv t `Set.difference` ftv env)




data TIEnv = TIEnv  {}

newtype TIState = TIState { tiSupply :: Int }

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t =
    do (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
       return (res, st)
  where initTIEnv = TIEnv
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





mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r')  =  do  s1 <- mgu l l'
                                    s2 <- mgu (apply s1 r) (apply s1 r')
                                    return (s1 `composeSubst` s2)
mgu (TVar u) t               =  varBind u t
mgu t (TVar u)               =  varBind u t
mgu TInt TInt                =  return nullSubst
mgu TBool TBool              =  return nullSubst
mgu t1 t2                    =  throwError $ "types do not unify: " ++ show t1 ++
                                " vs. " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t  | t == TVar u           =  return nullSubst
             | u `Set.member` ftv t  =  throwError $ "occurs check fails: " ++ u ++
                                         " vs. " ++ show t
             | otherwise             =  return (Map.singleton u t)





tiLit :: Lit -> TI (Subst, Type)
tiLit (LInt _)   =  return (nullSubst, TInt)
tiLit (LBool _)  =  return (nullSubst, TBool)
-- tiLit (LList []) =  return 



tiProg :: TypeEnv -> Program -> TI Subst
tiProg env (Program []) =
    return nullSubst

tiProg env (Program (def:defs)) = do
    (s, t, e) <- tiDef env def
    tiProg e (Program defs)


tiDef :: TypeEnv -> Def -> TI (Subst, Type, TypeEnv)
tiDef env (Definition name args e) = do
    tv <- newTyVar "a"


    throwError "Not yet implemented"



ti        ::  TypeEnv -> Exp -> TI (Subst, Type)
ti (TypeEnv env) (EVar n) =
    case Map.lookup n env of
       Nothing     ->  throwError $ "unbound variable: " ++ n
       Just sigma  ->  do  t <- instantiate sigma
                           return (nullSubst, t)
ti _ (ELit l) = tiLit l
ti env (EAbs n e) =
    do  tv <- newTyVar "a"
        let TypeEnv env' = remove env n
            env'' = TypeEnv (env' `Map.union` Map.singleton n (Scheme [] tv))
        (s1, t1) <- ti env'' e
        return (s1, TFun (apply s1 tv) t1)
ti env exp@(EApp e1 e2) =
    do  tv <- newTyVar "a"
        (s1, t1) <- ti env e1
        (s2, t2) <- ti (apply s1 env) e2
        s3 <- mgu (apply s2 t1) (TFun t2 tv)
        return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
    `catchError`
    \e -> throwError $ e ++ "\n in " ++ show exp

ti env (ELet x e1 e2) =
    do  (s1, t1) <- ti env e1
        let TypeEnv env' = remove env x
            t' = generalize (apply s1 env) t1
            env'' = TypeEnv (Map.insert x t' env')
        (s2, t2) <- ti (apply s1 env'') e2
        return (s1 `composeSubst` s2, t2)


ti env (ELetRec x e1 e2) = do
    let TypeEnv env' = remove env x
    tv <- newTyVar "a"
    let tv' = Scheme [] tv
    (s1, t1) <- ti (TypeEnv $ Map.insert x tv' env') e1

    -- (s1, t1) <- ti env e1
    let t' = generalize (apply s1 (TypeEnv env')) t1
    let env'' = TypeEnv (Map.insert x t' env')
    (s2, t2) <- ti (apply s1 env'') e2
    return (s1 `composeSubst` s2, t2)


ti env (EIf cond e1 e2) = do
    (sc, tc) <- ti env cond
    sc' <- mgu (apply sc tc) TBool

    (s1, t1) <- ti (apply sc' env) e1
    (s2, t2) <- ti (apply s1 env) e2
    s3 <- mgu (apply s2 t1) (apply s2 t2)

    return (s3 `composeSubst` s2 `composeSubst` s1 `composeSubst` sc', apply s3 t2)



ti env (EBinOp e1 op e2) = do
    let t = tiOpBin op
    (s1, t1) <- ti env e1
    (s2, t2) <- ti (apply s1 env) e2
    s3 <- mgu (apply s2 t1) (apply s2 t2)

    -- s3' <- mgu (apply s3 t2) t

    return (s3 `composeSubst` s2 `composeSubst` s1, t)

    -- old:
    -- s3' <- mgu (apply s3 t2) t
    -- return (s3' `composeSubst` s2 `composeSubst` s1, apply s3' t2)


ti env (EUnOp op e) = do
    let t = tiOpUn op
    (s1, t1) <- ti env e
    s2 <- mgu (apply s1 t1) t

    return (s2 `composeSubst` s1, apply s2 t1)







typeInference :: Map.Map String Scheme -> Exp -> TI Type
typeInference env e =
    do  (s, t) <- ti (TypeEnv env) e
        return (apply s t)



e0  =  ELet "id" (EAbs "x" (EVar "x"))
        (EVar "id")

e1  =  ELet "id" (EAbs "x" (EVar "x"))
        (EApp (EVar "id") (EVar "id"))

e2  =  ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
        (EApp (EVar "id") (EVar "id"))

e3  =  ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
        (EApp (EApp (EVar "id") (EVar "id")) (ELit (LInt 2)))

e4  =  ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x")))
        (EVar "id")

e5  =  EAbs "m" (ELet "y" (EVar "m")
                 (ELet "x" (EApp (EVar "y") (ELit (LBool True)))
                       (EVar "x")))

e6  =  EApp (ELit (LInt 2)) (ELit (LInt 2))




e7  = EIf (ELit $ LBool True) (ELit $ LInt 4) (ELit $ LInt 5)

e8  = EIf (ELit $ LBool True) (ELit $ LBool True) (ELit $ LInt 5)

e9  = EIf (ELit $ LInt 5) (ELit $ LBool True) (ELit $ LInt 5)




e10  = EIf (EBinOp  (ELit $ LBool True) OpOr (ELit $ LBool True)) (ELit $ LInt 4) (ELit $ LInt 5)

e11  = EIf (EBinOp (ELit $ LInt 4) OpOr (ELit $ LBool True)) (ELit $ LBool True) (ELit $ LInt 5)

e12 = EIf (ELit $ LInt 5) (ELit $ LBool True) (ELit $ LInt 5)


e13 = ELet "fun" (EAbs "x"
        (
            EIf (ELit $ LBool True)
                (EApp (EVar "fun") (ELit $ LInt 5))
                (ELit $ LInt 5)
        )
    )
    (
        EApp (EVar "fun") (ELit $ LInt 5)
    )

e14 = ELetRec "fun" (EAbs "x"
        (
            EIf (ELit $ LBool True)
                (EApp (EVar "fun") (ELit $ LInt 5))
                (ELit $ LInt 5)
        )
    )
    (
        EApp (EVar "fun") (ELit $ LInt 5)
    )

e15 = ELetRec "fun" (EAbs "x"
        (
            EIf (ELit $ LBool True)
                (EApp (EVar "fun") (EVar "x"))
                (EVar "x")
        )
    )
    (
        EApp (EVar "fun") (ELit $ LInt 5)
    )


e16 = ELetRec "fun" (EAbs "x"
        (
            EIf (ELit $ LBool True)
                (EApp (EVar "fun") (EVar "x"))
                (EVar "x")
        )
    )
    (
        EVar "fun"
    )

e17 = ELetRec "fun" (EAbs "x"
        (
            EIf (ELit $ LBool True)
                (EApp (EVar "fun") (ELit $ LBool True))
                (EVar "x")
        )
    )
    (
        EApp (EVar "fun") (ELit $ LInt 5)
    )


e18 = ELetRec "double" (EAbs "f"
        (
            EAbs "x"
            (
                EApp
                    (EVar "f")
                    (EApp
                        (EVar "f")
                        (EVar "x")
                    )
            )
        )
    )
    (
        -- EApp 
            (
                (EVar "double")
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


e19 = ELetRec "iter"
    (EAbs "f" (
        EAbs "x" (
            EAbs "n" (
                EIf (EVar "n")
                (EApp (EVar "f") (EVar "x"))
                (EApp (EApp (EVar "iter") (EVar "f")) (EVar "n"))
            )
        )
    ))
    (
        EVar "iter"
    )




test :: Exp -> IO ()
test e =
    do  (res, _) <- runTI (typeInference Map.empty e)
        case res of
          Left err  ->  putStrLn $ show e ++ "\n " ++ err ++ "\n"
          Right t   ->  putStrLn $ show e ++ " :: " ++ show t ++ "\n"





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

instance Show Exp where
    showsPrec _ x = shows (prExp x)

prExp                  ::  Exp -> PP.Doc
prExp (EVar name)      =   PP.text name
prExp (ELit lit)       =   prLit lit
prExp (ELet x b body)  =   PP.text "let" PP.<+>
                           PP.text x PP.<+> PP.text "=" PP.<+>
                           prExp b PP.<+> PP.text "in" PP.$$
                           PP.nest 2 (prExp body)


prExp (ELetRec x b body)  =   PP.text "letrec" PP.<+>
                           PP.text x PP.<+> PP.text "=" PP.<+>
                           prExp b PP.<+> PP.text "in" PP.$$
                           PP.nest 2 (prExp body)


prExp (EApp e1 e2)     =   prExp e1 PP.<+> prParenExp e2
prExp (EAbs n e)       =   PP.char '\\' PP.<> PP.text n PP.<+>
                           PP.text "->" PP.<+>
                           prExp e

-- TODO: this is ugly...
prExp (EIf c e1 e2)    =   PP.text "IF " PP.<+>  PP.text"(" PP.<+> prExp c PP.<+>  PP.text")  then"
                                        PP.<+>  PP.text"(" PP.<+> prExp e1 PP.<+>  PP.text")  else"
                                        PP.<+>  PP.text"(" PP.<+> prExp e2 PP.<+>  PP.text")  endif"

prExp (EBinOp e1 op e2) =  PP.text"(" PP.<+> prExp e1 PP.<+>  PP.text")"
                        PP.<+>  prOpBin op
                        PP.<+>  PP.text"(" PP.<+> prExp e2 PP.<+>  PP.text")"


prExp (EUnOp op e) = prOpUn op  PP.<+>  PP.text"(" PP.<+> prExp e PP.<+>  PP.text")"


prOpUn OpNeg = PP.text "-"
prOpUn OpNot = PP.text "~"


prOpBin (OpAdd _)= PP.text "+"
prOpBin (OpMul _)= PP.text "*"
prOpBin (OpRel _)= PP.text "<"
prOpBin OpAnd = PP.text "&&"
prOpBin OpOr = PP.text "||"



prParenExp    ::  Exp -> PP.Doc
prParenExp t  =   case t of
                    ELet _ _ _  -> PP.parens (prExp t)
                    EApp _ _    -> PP.parens (prExp t)
                    EAbs _ _    -> PP.parens (prExp t)
                    _           -> prExp t

instance Show Lit where
    showsPrec _ x = shows (prLit x)

prLit            ::  Lit -> PP.Doc
prLit (LInt i)   =   PP.integer i
prLit (LBool b)  =   if b then PP.text "True" else PP.text "False"
prLit (LList xs) =   PP.hcat (map prExp xs)

instance Show Scheme where
    showsPrec _ x = shows (prScheme x)

prScheme                  ::  Scheme -> PP.Doc
prScheme (Scheme vars t)  =   PP.text "All" PP.<+>
                              PP.hcat
                                (PP.punctuate PP.comma (map PP.text vars))
                              PP.<> PP.text "." PP.<+> prType t




