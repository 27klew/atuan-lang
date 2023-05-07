
module Atuan.TypeCheck where


import Atuan.Abs (Program, Type, Program' (ProgramText), Type' (..), Top' (TopDef, TopType), Def' (DefinitionT), Ident (Ident), OptTypeAnnot' (OptionalTypeAnnotation, SkippedTypeAnnotation), TypeAnnot' (TypeAnnotation), OTIdent' (OptionallyTypedIdentifier, SkippedTypeIdentifier), TIdent' (TypedIdentifier), Expr' (..), HasPosition (hasPosition), Val' (ValList))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, when)
import Atuan.CollectTypes (ADTs (ADTs))
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local))
import Atuan.CollectTypes (isType)
import Data.Map(Map, empty, fromList, union)



data TypeEnv a = TypeEnv {adts :: ADTs a, assigned :: Map Ident Type''}

fromADTs :: ADTs a -> TypeEnv a
fromADTs adts = TypeEnv adts empty

type RE a b = (ReaderT (TypeEnv b) (ExceptT String Identity)) a


typecheck :: Show a => ADTs a -> Atuan.Abs.Program' a -> Either String (Program' (a, Type' ()))
typecheck adts program = runIdentity (runExceptT (runReaderT ( typecheckProgram program) (fromADTs adts)))


type Type'' = Type' ()

cleanType :: Type' a -> Type''
cleanType (TypeInt _) = TypeInt ()
cleanType (TypeBool _) = TypeBool ()
cleanType (TypeList _ t) = TypeList () (cleanType t)
cleanType (TypeIdent _ i) = (TypeIdent () i)
cleanType (TypeApp _ name ts) = TypeApp () name (map cleanType ts)
cleanType (TypeFunc _ t1 t2) = TypeFunc () (cleanType t1) (cleanType t2)
cleanType (TypeVar _ name) = TypeVar () name



funTypeFromList :: [Type' a] -> Type' ()
funTypeFromList [] = error "Internal Error - Creating function type with empty type"
funTypeFromList [t] = cleanType t
funTypeFromList (t:ts) = TypeFunc () (cleanType t) (funTypeFromList ts)


otIdentToPair :: OTIdent' a -> (Ident, Type' ())
otIdentToPair (OptionallyTypedIdentifier a (TypedIdentifier _ name (TypeAnnotation _ ty))) = (name, cleanType ty)
otIdentToPair (SkippedTypeIdentifier _ _)  = error "This is not yet implemented"


typecheckVal :: Val' a -> RE (Val' (a, Type' ())) a
typecheckVal (ValList a expr) =  do
    expr' <- typecheckExpr expr
    let t = hasTypeExpr expr'
    return (ValList (a, t) expr')


hasTypeVal :: Val' (a, Type'') -> Type''
hasTypeVal (ValList a _) = snd a


typecheckExpr  :: Expr' a -> RE (Expr' (a, Type' ())) a
typecheckExpr (ELitInt a i) = return (ELitInt (a, TypeInt ()) i)
typecheckExpr (ELitBool a b) = return (ELitBool (a, TypeBool ()) b)

typecheckExpr (ELambda a _ ) = throwError "Not yet implemented expr kind"

typecheckExpr (ELitList a vals) = do
    vals' <- mapM typecheckVal vals
    let types = map hasTypeVal vals'

    compatibleList types


    -- TODO: empty lists should be handled better

    case types of
      [] -> return $ ELitList (a, TypeVar () (Ident "a")) vals'
      ty : _ -> return $ ELitList (a, TypeList () ty) vals'




typecheckExpr (EApp a _ _ ) = throwError "Not yet implemented expr kind"
typecheckExpr (Neg a _) = throwError "Not yet implemented expr kind"
typecheckExpr (Not a _ ) = throwError "Not yet implemented expr kind"
typecheckExpr (EMul a _ _ _) = throwError "Not yet implemented expr kind"
typecheckExpr (EAdd a _ _ _) = throwError "Not yet implemented expr kind"
typecheckExpr (ERel a _ _ _ ) = throwError "Not yet implemented expr kind"
typecheckExpr (EAnd a _ _) = throwError "Not yet implemented expr kind"
typecheckExpr (EOr a _ _ ) = throwError "Not yet implemented expr kind"
typecheckExpr (EMatch a _ _ ) = throwError "Not yet implemented expr kind"
typecheckExpr (EIf a _ _ _ ) = throwError "Not yet implemented expr kind"
typecheckExpr (ELet a _ _) = throwError "Not yet implemented expr kind"
typecheckExpr (EVar a _) = throwError "Not yet implemented expr kind"


compatible :: Type' () -> Type' () -> RE () a
compatible t1 t2 = do
    when (t1 /= t2)
        (throwError $ "Types " ++ show t1 ++ ",  " ++ show t2 ++ "are not compatible.")


compatibleList :: [Type''] -> RE () a
compatibleList [t1, t2] = compatible t1 t2
compatibleList (t1:t2:t3:ts) = do
    compatible t1 t2
    compatible t2 t3
    compatibleList (t3:ts)
compatibleList _ = return ()


hasTypeExpr :: Expr' (a, Type'') -> Type''
hasTypeExpr (ELitInt a _) = snd a
hasTypeExpr (ELitBool a _) = snd a
hasTypeExpr (ELambda a _ ) = snd a
hasTypeExpr (ELitList a _) = snd a
hasTypeExpr (EApp a _ _ ) = snd a
hasTypeExpr (Neg a _) = snd a
hasTypeExpr (Not a _ ) = snd a
hasTypeExpr (EMul a _ _ _) = snd a
hasTypeExpr (EAdd a _ _ _) = snd a
hasTypeExpr (ERel a _ _ _ ) = snd a
hasTypeExpr (EAnd a _ _) = snd a
hasTypeExpr (EOr a _ _ ) = snd a
hasTypeExpr (EMatch a _ _ ) = snd a
hasTypeExpr (EIf a _ _ _ ) = snd a
hasTypeExpr (ELet a _ _) = snd a
hasTypeExpr (EVar a _) = snd a





typecheckDef  :: Def' a -> RE (Def' (a, Type' ())) a
typecheckDef (DefinitionT a ide otidents otype expr) = do
    let (OptionalTypeAnnotation _ (TypeAnnotation _ res_type))  = otype -- TODO | SkippedTypeAnnotation a
    let args =  map otIdentToPair otidents
    let fun_type = funTypeFromList (map snd args ++ [cleanType res_type])
    let args' = fromList $ (ide, fun_type):args


    expr' <- local (\(TypeEnv adts assign) -> TypeEnv adts (args' `union` assign))
        (
             typecheckExpr expr
        )

    let t = hasTypeExpr expr'

    compatible (cleanType res_type) t

    -- Use a ne AST here - skip the otype, but keep otidents (in sensible form) 
    return (DefinitionT (a, cleanType res_type) ide [] (SkippedTypeAnnotation (a, TypeInt ())) expr')







typecheckTop  :: Top' a -> RE (Top' (a, Type' ())) a
typecheckTop (TopType a typedef) = throwError "Internal Error: There should be no types here."
typecheckTop (TopDef a def) = do
    def' <- typecheckDef def
    return (TopDef (a, TypeInt ()) def')



typecheckProgram :: Program' a -> RE (Program' (a, Type' ())) a
typecheckProgram (ProgramText p tops) = do
    tops' <- mapM typecheckTop (filter (not . isType) tops)
    let res = ProgramText (p, TypeInt ()) tops'
    return res

