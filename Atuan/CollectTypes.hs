
module Atuan.CollectTypes where

import Atuan.Abs (Program'(..), Top'(..), TypeDef'(..), Ident(..), TVar'(..), Constr'(..), TVar, Type' (TypeFunc, TypeIdent, TypeApp, TypeInt, TypeBool, TypeList, TypeVar), TypeAnnot' (TypeAnnotation))
import Control.Monad.Reader (Reader, ReaderT, MonadReader (ask), MonadTrans (lift))
import Control.Monad.Except (ExceptT, runExceptT, when, MonadError (throwError), unless, Except)
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.State.Strict (modify, put, get, StateT, execStateT, runState, evalState, evalStateT)
import Data.Map (Map, insert, empty, keys, member, elems, toList, fromList, union, lookup)
import Data.Char (isUpper, isLower)
import GHC.Conc (TVar(TVar))
import qualified Data.Set as Set (fromList)
import Data.List (sort)
import Data.Maybe (isNothing)
-- import Prelude (Maybe(..), Eq, Either(..), String, Show, Bool, mapM_, (==), )



-- TODO something other here
-- data Type = Int | Bool | Func Type Type/


-- type TypeEnv a = Map Ident ([TVar' a], [Constr' a])


data ADT a = ADT {name :: Ident, vars :: [TVar' a], constrs :: [Ident]} deriving (Show, Eq, Ord)


data ADTs a = ADTs {from_name :: Map Ident (ADT a) , from_constr :: Map Ident (Constr' a)} deriving (Show, Eq, Ord)


type SE a b = (StateT (ADTs b) (ExceptT String Identity)) a

-- type RE a b = (ReaderT (ADTs b) (ExceptT String Identity)) a



-- note :: e -> Maybe a -> Either e a
-- note e Nothing  = Left e
-- note _ (Just a) = Right a


note :: e -> Maybe a -> Except e a
note e Nothing  = throwError e
note _ (Just a) = return a


getConstr :: Ident -> SE (Constr' a) a
getConstr i = do
  adts <- get
  constr <- lift $ note ("No such constructor" ++ show i) (Data.Map.lookup i (from_constr adts))
  return constr


getType :: Ident -> SE (ADT a) a
getType i = do
  adts <- get
  adt <- lift $ note ("No such type " ++ show i) (Data.Map.lookup i (from_name adts))
  return adt



emptyADTs = ADTs empty  empty



collect :: Show a => Atuan.Abs.Program' a -> Either String (ADTs a)
collect program = runIdentity (runExceptT (execStateT ( collectProgram program) emptyADTs))


isType :: Top' a -> Bool
isType (TopType _ _ ) = True
isType _ = False

topName :: Top' a -> Ident
topName (TopType _ (TypeDefinition _ id _ _ )) = id
-- topNamae (topDef _ _ ) TODO 


collectProgram :: Show a => Atuan.Abs.Program' a -> SE () a
collectProgram (Atuan.Abs.ProgramText ann tops) = do
    let types = filter isType tops
    let names = map topName types
    let names' = sort names


    let x = findDuplicate names'
    unless (isNothing x)
      (throwError $ "Duplicate typename " ++ show x) -- TODO locations


    let y = filter (not . isUpperIdent) names'
    unless (null x)
      (throwError $ "Typename should start with capital letters:" ++ show x)



    mapM_ collectType types
    collectType (builtInList ann)

    types' <- get

    -- TODO
    -- let con = map snd (elems types)
    -- let con' = map (\(DataConstructor _ id _) -> id) (concat con)
    -- let con'' = sort con'
    -- checkConstructors con''

    return ()

builtInList dummy =
  TopType dummy (Atuan.Abs.TypeDefinition dummy (Ident "List") vars constr)
  where vars = [TypeVariable dummy (Ident "ltype")]
        constr = [
          DataConstructor dummy (Ident "Empty")  (TypeAnnotation dummy (TypeApp dummy (Ident "List") [TypeVar dummy (Ident "any")])),
          DataConstructor dummy (Ident "Cons")  (TypeAnnotation dummy
            (TypeFunc dummy (TypeVar dummy (Ident "any")) (TypeFunc dummy (TypeVar dummy (Ident "any")) (TypeApp dummy (Ident "List") [TypeVar dummy (Ident "any")])) )
            )

          -- DataConstructor dummy (Ident "cons") (TypeAnnot' a),
          -- DataConstructor a Ident (TypeAnnot' a)
          ]

collectType :: Show a => Atuan.Abs.Top' a -> SE () a
collectType (TopDef _ _ ) = return ()
collectType (TopType pos (Atuan.Abs.TypeDefinition _ ident vars constr)) = do
    checkTypeName ident
    mapM_ checkTypeVar vars
    mapM_ (checkDataConstr ident vars) constr
    constr' <-  mapM identToVarConstr constr

    m <- get

    --  TODO checkIdentUnique ident m

    addType ident vars constr'

-- TODO builtin List should be first

identConstr :: Constr' a -> Ident
identConstr (DataConstructor _ i _) = i

identType :: Constr' a -> Type' a
identType (DataConstructor _ _ (TypeAnnotation _ t)) = t



addConstr :: Constr' a -> SE () a
addConstr constr = do
  (ADTs types con) <- get
  let name = identConstr constr
  let con' = insert name constr con
  put (ADTs types con')


addType :: Ident -> [TVar' a] -> [Constr' a]  -> SE () a
addType  name vars constr = do
  (ADTs types con) <- get

  let conide = map identConstr constr

  let adt = ADT name vars conide

  let types' = insert name adt types

  put (ADTs types' con)

  mapM_ addConstr constr







-- isConstr ::  Ident -> RE Ident b
-- isConstr i = do
--   env <- get
--   -- let l = toList env
--   -- let li = map (\(x, y) -> (x, identConstr (snd y))) l
--   -- let l' = filter (\(_, x) -> elem i x) li

--   -- when (null l')
--   --   (throwError $ "No such constructor " ++ show i)


--   throwError "aaa"







findDuplicate :: Eq a => [a] -> Maybe a
findDuplicate [] = Nothing
findDuplicate [_] = Nothing
findDuplicate (x:y:xs) =
  if x == y then Just x else findDuplicate (y:xs)



checkConstructors :: [Ident] -> SE () a
checkConstructors con = do
  let dup = findDuplicate con
  unless (isNothing dup)
   ( throwError $ "Data constructors should be unique: " ++ show dup )


-- collectProgram :: Show a => Atuan.Abs.Program' a -> RE () a
-- collectProgram (Atuan.Abs.ProgramText ann tops) = do
--     mapM_ collectType tops
--     types <- get

--     -- TODO
--     -- let con = map snd (elems types)
--     -- let con' = map (\(DataConstructor _ id _) -> id) (concat con)
--     -- let con'' = sort con'
--     -- checkConstructors con''

--     return ()


isUpperIdent :: Ident -> Bool
isUpperIdent (Ident ident) =  isUpper (head ident)

isLowerIdent :: Ident -> Bool
isLowerIdent (Ident ident) = isLower (head ident)


checkTypeName :: Show a => Ident  -> SE () a
checkTypeName ident = do
    unless (isUpperIdent ident)
         (throwError $  "Error: type names should begin with capital letters. "
            ++ "offending name:" ++ show ident
         )


checkTypeVar :: Show a => TVar' a -> SE () a
checkTypeVar (TypeVariable pos ident)  = do
    unless (isLowerIdent ident)
        (throwError $ "Error: type variables should begin with small letter."
            ++ "offending name: " ++  show ident  ++ "at: " ++ show pos
        )


getResultType :: Type' a -> Type' a
getResultType (TypeFunc _ t1 t2) = getResultType t2
getResultType t = t


checkDataConstr :: Show a => Ident -> [TVar' a] -> Constr' a -> SE () a
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
    if isLowerIdent id then
        -- error "Whhhhooo"
        return $ TypeVar a id
    else
        return $ TypeIdent a id

  TypeVar a id -> do
    checkTypeVar (TypeVariable a id)
    return $ TypeVar a id


identToVarConstr :: Show a => Constr' a -> SE (Constr' a) a
identToVarConstr (DataConstructor pos ident (TypeAnnotation ps ty)) = do
    ty' <- identToVar ty
    adts <- get
    let constrs = keys $ from_constr adts
    when (ident `elem` constrs)
      (throwError $ "Multiple declarations of constructor " ++ show ident ++ " at position " ++ show pos)
    return (DataConstructor pos ident (TypeAnnotation ps ty'))


checkIdentUnique :: Ident -> Map Ident a -> SE () b
checkIdentUnique i m =
  when (member i m)
    (throwError $ "Typename " ++ show i ++ "is not unique")








