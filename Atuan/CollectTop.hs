
module Atuan.CollectTop where

import Atuan.Abs (Program'(..), Top'(..), TypeDef'(..), Ident(..), TVar'(..), Constr'(..), TVar, Type' (TypeFunc, TypeIdent, TypeApp, TypeInt, TypeBool, TypeList, TypeVar), TypeAnnot' (TypeAnnotation))
import Control.Monad.Reader (Reader, ReaderT)
import Control.Monad.Except (ExceptT, runExceptT, when, MonadError (throwError), unless)
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.State.Strict (modify, put, get, StateT, execStateT, runState, evalState, evalStateT)
import Data.Map (Map, insert, empty)
import Data.Char (isUpper, isLower)
import GHC.Conc (TVar(TVar))


-- TODO something other here
data Type = Int | Bool | Func Type Type

type TypeEnv a = Map Ident ([TVar' a], [Constr' a])


-- type SS a = ReaderT TypeEnv (ExceptT String Identity) a

type RE a b = (StateT (TypeEnv b) (ExceptT String Identity)) a


collect :: Show a => Atuan.Abs.Program' a -> Either String (TypeEnv a)
collect program = runIdentity (runExceptT (execStateT ( collectProgram program) empty))


collectProgram :: Show a => Atuan.Abs.Program' a -> RE () a
collectProgram (Atuan.Abs.ProgramText ann tops) = do
    mapM_ collectTypes tops



isUpperIdent :: Ident -> Bool
isUpperIdent (Ident ident) =  isUpper (head ident)

isLowerIdent :: Ident -> Bool
isLowerIdent (Ident ident) = isLower (head ident)


checkTypeName :: Show a => Ident  -> RE () a
checkTypeName ident = do
    unless (isUpperIdent ident)
         (throwError $  "Error: type names should begin with capital letters. "
            ++ "offending name:" ++ show ident
         )


checkTypeVar :: Show a => TVar' a -> RE () a
checkTypeVar (TypeVariable pos ident)  = do
    unless (isLowerIdent ident)
        (throwError $ "Error: type variables should begin with small letter."
            ++ "offending name: " ++  show ident  ++ "at: " ++ show pos
        )


getResultType :: Type' a -> Type' a
getResultType (TypeFunc _ t1 t2) = getResultType t2
getResultType t = t


checkDataConstr :: Show a => Ident -> [TVar' a] -> Constr' a -> RE () a
checkDataConstr id vars (DataConstructor pos ident (TypeAnnotation _ ty)) = do
    checkTypeName ident
    let t = getResultType ty
    case t of
      TypeIdent a id' -> do
        when (id' /= id)
            (throwError $ "Data constructor should construct value in type " ++ show id ++ " at " ++ show a)
        unless (null vars)
            (throwError $ "Data constructor should specialize all type variables " ++ show id'  ++ " at " ++ show a)

      TypeApp a id' tys -> do
        when (id' /= id)
            (throwError $ "Data constructor should construct value in type " ++ show id ++ " at " ++ show a)

        when (length tys /= length vars)
            (throwError $ "Data constructor should specialize all type variables " ++ show id'  ++ " at " ++ show a)

      t -> throwError  $ "Data constructor should result in creation of value in type " ++ show id ++ " received " ++ show t

    return ()




identToVar :: Show a => Type' a -> RE (Type' a) a 
identToVar x = case x of
  TypeInt a -> return $ TypeInt a
  TypeBool a -> return $ TypeBool a
  TypeList a ty -> do 
    ty' <- identToVar ty 
    return $ TypeList a ty'
  TypeApp a id tys -> do
    checkTypeName id    
    tys' <- mapM identToVar tys
    return $ TypeApp a id tys'

  TypeFunc a ty1 ty2 -> do
    ty1' <- identToVar ty1
    ty2' <- identToVar ty2
    return $ TypeFunc a ty1' ty2'

  TypeIdent a id -> 
    if isLowerIdent id then 
        -- error "Whhhhooo"
        return $ TypeVar a id
    else
        return $ TypeIdent a id

  TypeVar a id -> do 
    checkTypeVar (TypeVariable a id)
    return $ TypeVar a id


identToVarConstr :: Show a => Constr' a -> RE (Constr' a) a
identToVarConstr (DataConstructor pos ident (TypeAnnotation ps ty)) = do
    ty' <- identToVar ty
    return (DataConstructor pos ident (TypeAnnotation ps ty'))


collectTypes :: Show a => Atuan.Abs.Top' a -> RE () a
collectTypes (TopDef _ _ ) = return ()
collectTypes (TopType pos (Atuan.Abs.TypeDefinition _ ident vars constr)) = do

    checkTypeName ident
    mapM_ checkTypeVar vars
    mapM_ (checkDataConstr ident vars) constr
    constr' <-  mapM identToVarConstr constr

    modify (insert ident (vars, constr'))



-- x :: Type' ()
-- x = TypeIdent () (Ident "aaaaa")

-- -- >>> isLowerIdent (Ident "aaaa")
-- -- True

-- y = identToVar x

-- y1 = runIdentity $ runExceptT (evalStateT y empty)

-- >>> y1
-- Right (TypeVar () (Ident "aaaaa"))



