module Atuan.CollectTypes (collect, ADTs (..), isType, ADT (..)) where

import Atuan.Abs (Constr' (..), Ident (..), Program' (..), TVar, TVar' (..), Top' (..), Type' (TypeApp, TypeBool, TypeFunc, TypeIdent, TypeInt, TypeList, TypeVar), TypeAnnot' (TypeAnnotation), TypeDef' (..))
import Control.Monad.Except (Except, ExceptT, MonadError (throwError), runExceptT, unless, when)
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), Reader, ReaderT)
import Control.Monad.State.Strict (StateT, evalState, evalStateT, execStateT, get, modify, put, runState)
import Data.Char (isLower, isUpper)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (sort)
import Data.Map (Map, elems, empty, fromList, insert, keys, lookup, member, toList, union)
import Data.Maybe (isNothing)
import qualified Data.Set as Set (fromList)
import GHC.Conc (TVar (TVar))
import qualified Data.Set

data ADT a = ADT
  { name :: Ident,
    vars :: [TVar' a],
    constrs :: [Ident]
  }
  deriving (Show, Eq, Ord)

data ADTs a = ADTs
  { from_name :: Map Ident (ADT a),
    from_constr :: Map Ident (Constr' a)
  }
  deriving (Show, Eq, Ord)

type SE a b = (StateT (ADTs b) (ExceptT String Identity)) a

-----------------------------------------------------------------------
--                   MAIN COLLECTION FUNCTION                        --
-----------------------------------------------------------------------

collect :: Show a => Atuan.Abs.Program' a -> Either String (ADTs a)
collect program = runIdentity (runExceptT (execStateT (collectProgram program) emptyADTs))

collectProgram :: Show a => Atuan.Abs.Program' a -> SE () a
collectProgram (Atuan.Abs.ProgramText ann tops) = do
  let types = filter isType tops

  mapM_ collectType (builtInList ann : types)

  types' <- get
  let con = elems $ from_constr types'
  mapM_ checkConstructor con

note :: e -> Maybe a -> Except e a
note e Nothing = throwError e
note _ (Just a) = return a

getConstr :: Ident -> SE (Constr' a) a
getConstr i = do
  adts <- get
  lift $ note ("No such constructor" ++ show i) (Data.Map.lookup i (from_constr adts))

getType :: Ident -> SE (ADT a) a
getType i = do
  adts <- get
  lift $ note ("No such type " ++ show i) (Data.Map.lookup i (from_name adts))

emptyADTs = ADTs empty empty

isType :: Top' a -> Bool
isType (TopType _ _) = True
isType _ = False

topName :: Top' a -> Ident
topName (TopType _ (TypeDefinition _ id _ _)) = id
topName (TopDef _ _) = error "Type Collection should not be called on definition."

builtInList dummy =
  TopType dummy (Atuan.Abs.TypeDefinition dummy (Ident "List") vars constr)
  where
    vars = [TypeVariable dummy (Ident "ltype")]
    constr =
      [ DataConstructor dummy (Ident "Empty") (TypeAnnotation dummy (TypeApp dummy (Ident "List") [TypeVar dummy (Ident "any")])),
        DataConstructor
          dummy
          (Ident "Cons")
          ( TypeAnnotation
              dummy
              (TypeFunc dummy (TypeVar dummy (Ident "any")) (TypeFunc dummy (TypeVar dummy (Ident "any")) (TypeApp dummy (Ident "List") [TypeVar dummy (Ident "any")])))
          )
          -- DataConstructor dummy (Ident "cons") (TypeAnnot' a),
          -- DataConstructor a Ident (TypeAnnot' a)
      ]

collectType :: Show a => Atuan.Abs.Top' a -> SE () a
collectType (TopDef _ _) = return ()
collectType (TopType pos (Atuan.Abs.TypeDefinition _ ident vars constr)) = do
  checkTypeName ident
  mapM_ checkTypeVar vars
  let vars' = map varName vars
  let v = findDuplicate vars'
  unless (v == Nothing) 
    (throwError $ "Duplicate variable " ++ show v ++ "at position " ++ show pos)

  constr' <- mapM identToVarConstr constr
  mapM_ (checkDataConstr ident vars) constr'

  m <- get

  --  TODO checkIdentUnique ident m

  addType ident vars constr'

identConstr :: Constr' a -> Ident
identConstr (DataConstructor _ i _) = i

identType :: Constr' a -> Type' a
identType (DataConstructor _ _ (TypeAnnotation _ t)) = t

varName :: TVar' a -> String
varName (TypeVariable a (Ident s)) = s

checkConstructor :: Show a => Constr' a -> SE () a
checkConstructor (DataConstructor a id (TypeAnnotation a' ty)) = checkType ty

checkType :: Show a => Type' a -> SE () a
checkType t = case t of
  TypeInt a -> return ()
  TypeBool a -> return ()
  TypeList a ty ->
    checkType ty
  TypeIdent a id -> do
    adts <- get
    let adt = Data.Map.lookup id (from_name adts)
    case adt of
      Nothing -> throwError $ "Unknown type: " ++ show id
      Just (ADT id' tvs ids) ->
        ( do
            unless
              (null tvs)
              (throwError $ "Type " ++ show id ++ "requires type variables (at" ++ show a ++ ")")
        )
  TypeApp a id tys -> do
    adts <- get
    let adt = Data.Map.lookup id (from_name adts)
    case adt of
      Nothing -> throwError $ "Unknown type: " ++ show id
      Just (ADT id' tvs ids) ->
        ( do
            unless
              (length tvs == length tys)
              (throwError $ "Mismatch on type variable count for type " ++ show id ++ "at " ++ show a)
            mapM_ checkType tys
        )
  TypeVar a id -> return ()
  TypeFunc a ty ty' -> do
    checkType ty
    checkType ty'

addConstr :: Constr' a -> SE () a
addConstr constr = do
  (ADTs types con) <- get
  let name = identConstr constr
  let con' = insert name constr con
  put (ADTs types con')

addType :: Ident -> [TVar' a] -> [Constr' a] -> SE () a
addType name vars constr = do
  (ADTs types con) <- get

  let conide = map identConstr constr

  let adt = ADT name vars conide

  when
    (member name types)
    (throwError $ "Multiple definitions of : " ++ show name)

  let types' = insert name adt types

  put (ADTs types' con)

  mapM_ addConstr constr

findDuplicate :: Eq a => [a] -> Maybe a
findDuplicate [] = Nothing
findDuplicate [_] = Nothing
findDuplicate (x : y : xs) =
  if x == y then Just x else findDuplicate (y : xs)

isUpperIdent :: Ident -> Bool
isUpperIdent (Ident ident) = isUpper (head ident)

isLowerIdent :: Ident -> Bool
isLowerIdent (Ident ident) = isLower (head ident)

checkTypeName :: Show a => Ident -> SE () a
checkTypeName ident = do
  unless
    (isUpperIdent ident)
    ( throwError $
        "Error: type names should begin with capital letters. "
          ++ "offending name:"
          ++ show ident
    )

checkTypeVar :: Show a => TVar' a -> SE () a
checkTypeVar (TypeVariable pos ident) = do
  unless
    (isLowerIdent ident)
    ( throwError $
        "Error: type variables should begin with small letter."
          ++ "offending name: "
          ++ show ident
          ++ "at: "
          ++ show pos
    )

getResultType :: Type' a -> Type' a
getResultType (TypeFunc _ t1 t2) = getResultType t2
getResultType t = t

getVars :: Type' a -> Data.Set.Set Ident
getVars t = case t of
  TypeInt a -> Data.Set.empty
  TypeBool a -> Data.Set.empty
  TypeList a ty -> getVars ty
  TypeIdent a id -> Data.Set.empty
  TypeApp a id tys -> Data.Set.unions (map getVars tys)
  TypeVar a id -> Data.Set.singleton id
  TypeFunc a ty ty' -> getVars ty `Data.Set.union` getVars ty'

checkDataConstr :: Show a => Ident -> [TVar' a] -> Constr' a -> SE () a
checkDataConstr id vars (DataConstructor pos ident (TypeAnnotation _ ty)) = do
  checkTypeName ident

  let t = getResultType ty

  case t of
    TypeIdent a id' -> do
      when
        (id' /= id)
        (throwError $ "Data constructor should construct value in type " ++ show id ++ " at " ++ show a)
      unless
        (null vars)
        (throwError $ "Data constructor should specialize all type variables " ++ show id' ++ " at " ++ show a)
    TypeApp a id' tys -> do
      when
        (id' /= id)
        (throwError $ "Data constructor should construct value in type " ++ show id ++ " at " ++ show a)

      when
        (length tys /= length vars)
        (throwError $ "Data constructor should specialize all type variables " ++ show id' ++ " at " ++ show a)
    t -> throwError $ "Data constructor should result in creation of value in type " ++ show id ++ " received " ++ show t

  let tvs = getVars ty 
  let tvr = getVars t


  unless (tvs `Data.Set.isSubsetOf` tvr)
    (throwError $ 
      "GADTs with variables not present in result re not supported. offending varibles: " 
          ++ show (tvs `Data.Set.difference` tvr)
          )



identToVar :: Show a => Type' a -> SE (Type' a) a
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
    if isLowerIdent id
      then return $ TypeVar a id
      else return $ TypeIdent a id
  TypeVar a id -> do
    checkTypeVar (TypeVariable a id)
    return $ TypeVar a id

identToVarConstr :: Show a => Constr' a -> SE (Constr' a) a
identToVarConstr (DataConstructor pos ident (TypeAnnotation ps ty)) = do
  ty' <- identToVar ty
  adts <- get
  let constrs = keys $ from_constr adts
  when
    (ident `elem` constrs)
    (throwError $ "Multiple declarations of constructor " ++ show ident ++ " at position " ++ show pos)
  return (DataConstructor pos ident (TypeAnnotation ps ty'))

checkIdentUnique :: Ident -> Map Ident a -> SE () b
checkIdentUnique i m =
  when
    (member i m)
    (throwError $ "Typename " ++ show i ++ "is not unique")
