module Atuan.Evaluate where


import Atuan.AlgorithmW (Exp(..), Lit (..), OpBin (..), MulOp (..), AddOp (..), RelOp (..), OpUn (..), PatternBranch (..), Pattern (..))

import Data.Map(Map(..), member, lookup, union, fromList, insert, empty, toList, unions)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local), runReader, asks)
import Control.Monad.State (StateT (runStateT), MonadState (..), evalStateT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Except (ExceptT, throwError, unless, runExceptT, MonadError (catchError))
import Atuan.CollectTypes (ADTs (..))
import qualified Atuan.Abs
import Atuan.Abs (Constr' (..), HasPosition)
import Atuan.Translate (iname, itname, Pos)
import Debug.Trace
import Control.Monad (zipWithM)


-- type Env =  Data.Map.Map String Loc

type ValEnv = Data.Map.Map String Val

data Env = Env {getVals :: ValEnv, getADTs :: ADTs Pos} deriving (Eq, Ord, Show)

data Val' a = VInt Integer
    | VBool Bool
    | VFun String Env (Exp a)
    | VADT String [Val' a]
    | VExp (Exp a) Env deriving (Eq, Ord, Show)

type Val = Val' Pos

type Expected a = ExceptT String a

type EM a = ReaderT Env ( Expected Identity) a


-- constrToName :: Constr' a -> String
-- constrToName con = case con of { DataConstructor a (Atuan.Abs.Ident id) ta -> id}

setupEnv :: ADTs Pos -> Env
setupEnv adts =
    let constr = from_constr adts in
        let cs = Data.Map.toList constr
            in let names = map (itname . fst) cs in
                foldr (flip f) (Env Data.Map.empty adts) names

f :: Env -> String -> Env
f env name =
    Env (insert name (VADT name []) (getVals env)) (getADTs env)


testEval :: ADTs Pos -> Exp Pos -> Either String (Val)
testEval adts exp =
    let x = do
        exp' <- eval exp :: EM (Val)
        normal exp'
    in
    let env = setupEnv adts in
    let y = runReaderT x env in
    let w = runExceptT y in
    let q = runIdentity w in
        q
    -- let v = runReaderT Data.Map.empty (runStateT (Data.Map.empty, 0) (runExceptT (eval exp))) in
        -- v



-- setNew :: String -> Val' -> EM Loc
-- setNew s v = do
--     l <- newlock
--     (mem, nnnn) <- get
--     insert l n 


-- TODO --- this should probably have it's  own value
normal :: (Val) -> EM (Val)
normal v = case v of
  VExp exp env -> do
        v' <- local (const env)(eval exp)
        normal v'

  v' -> return v'


askVals :: EM ValEnv
askVals = do asks getVals



askVal' :: String -> EM Val
askVal' s = do
    vals <- askVals

    case Data.Map.lookup s vals of
      Nothing -> throwError $ "Unknown variable: " ++ show s
      Just val -> return val


unionVal :: String -> Val -> Env -> Env
unionVal s v (Env env adts) =
  Env (union (fromList [(s, v)]) env) adts


bindVal :: String -> Val -> EM a -> EM a
bindVal s v m = do
  env <- ask
  let env' = unionVal s v env
  local (const env') m


-- setup :: ADTs b -> EM () b
-- setup adts = do
--     let x = (from_name adts) 
--     throwError "aaa"

-- instance HasPosition a => HasPosition (Val' a) where
--   -- hasPosition :: Val' a -> Pos
--   hasPosition v = case v of
--     VInt n -> _
--     VBool b -> _
--     VFun s map exp -> _
--     VADT s vals -> _
--     VExp exp map -> _


eval :: Exp Pos -> EM (Val)
eval exp = case exp of
  EVar pos s -> do
    askVal' s

  ELit _ lit -> case lit of
    LInt pos n -> return $ VInt n
    LBool pos b -> return $ VBool b
    LList pos exps -> do
        vals <- mapM eval exps
        return (foldr (\x a-> VADT "Cons" [x, a]) (VADT "Empty" [])  vals)


  EApp pos exp' exp2 -> do
    f <- evalNorm exp'
    x <- evalNorm exp2

    case f of
      VFun s map exp3 -> do
          -- l <- newlock
          -- (mem, n, a) <- get
          -- env <- ask

          -- put (insert l x mem, n, a)


          local (const $ unionVal s x map) (eval exp3)
          -- bindVal s x (eval exp3)

          -- local (union (fromList [(s, l)] `union` map)) (eval exp3)

      VADT s vs ->
        return $ VADT s (vs ++ [x])

      _ -> throwError "Runtime Error: EApp on non-function argument. This should not happen TODO"


  EAbs pos s exp' -> do
    env <- ask
    return $ VFun s env exp'


  ELet pos s exp' exp2 -> do
    v <- eval exp'
    -- l <- newlock
    -- (mem, n, a) <- get
    -- put (insert l v mem, n, a)

    bindVal s v (eval exp2)

    -- local (union (fromList [(s, l)])) (eval exp2)


  ELetRec pos s exp' exp2 -> do
    -- l <- newlock
    -- (mem, n, a) <- get
    env <- ask

    -- TODO tutaj jest jakaś wartość zależna sama od siebie, 
    -- trzeba to jakoś naprawić, bo to powoduje głupie zachowanie np przy częściowej aplikacji
    -- wcześniej to było rozdzielone na wartość i lokację więc było ok
    -- może powinno przechowywać stare środowisko (bez siebie) i swoją nazwę i poprawiać na bieżąco?

    

    let v = VExp exp' (unionVal s v env) -- (fromList [(s, v)] `union` env )

    -- put (insert l v mem, n, a)

    bindVal s v (eval exp2)
    -- local (union (fromList [(s, l)])) (eval exp2)

  EIf pos exp' exp2 exp3 -> do
    cond <- evalNorm exp'

    case cond of
      VBool b -> evalNorm (if b then exp2 else exp3)
      _ -> throwError "Incorrect type of value. this should not happen."


  EBinOp pos exp' ob exp2 -> do
    case ob of
      OpMul mo -> (do
        v1 <- evalNorm exp'
        v2 <- evalNorm exp2

        let VInt v1' = v1
        let VInt v2' = v2

        (case mo of
            Times -> return (VInt $ v1' * v2')
            Div -> if v2' == 0 then throwError ("Error: Division by zero. at " ++ show pos)
                    else return (VInt $ v1' `div` v2')
            Mod -> if v2' == 0 then throwError ("Error: Modulo by zero  at " ++ show pos)
                    else return (VInt $ v1' `mod` v2')
            ))
      OpAdd ao -> (do
        v1 <- evalNorm exp'
        v2 <- evalNorm exp2

        let VInt v1' = v1
        let VInt v2' = v2

        case ao of
          Plus -> return (VInt $ v1' + v2')
          Minus -> return (VInt $ v1' - v2')
        )
      OpRel ro -> ( do
        v1 <- evalNorm exp'
        v2 <- evalNorm exp2

        let VInt v1' = v1
        let VInt v2' = v2

        return $ VBool (case ro of
          LTH ->  v1 < v2
          LE -> v1 <= v2
          GTH -> v1 > v2
          GE -> v1 >= v2
          EQU -> v1 == v2
          NE -> v1 /= v2)

        )
      OpAnd -> ( do
        v1 <- evalNorm exp'
        -- v2 <- evalNorm exp2

        let VBool v1' = v1
        -- let VInt v2' = v2 

        if v1' then
            evalNorm exp2
        else
            return $ VBool False
        )

      OpOr -> ( do
        v1 <- evalNorm exp'
        -- v2 <- evalNorm exp2

        let VBool v1' = v1
        -- let VInt v2' = v2 

        if v1' then
            return $ VBool True
        else
            evalNorm exp2
        )


  EUnOp pos ou exp' -> case ou of
    OpNeg -> ( do
            v <- evalNorm exp'
            let VInt v' = v
            return $ VInt (-v')
        )
    OpNot -> ( do
            v <- evalNorm exp'
            let VBool v' = v
            return $ VBool (not v')
        )


  EMatch pos s pbs -> do
    v <- evalNorm (EVar pos s)
    evalBranches v pbs
    -- `catchError`
    -- (++ "aaa")



evalBranches :: Val -> [PatternBranch Pos] -> EM (Val)
evalBranches v [] = throwError $ "Pattern match non-exhaustive on value " ++ show v
evalBranches v (p:ps) = do
      evalBranch v p `catchError` const (evalBranches v ps)



-- TODO: Throw error if adts different?
unionEnv :: ValEnv -> Env -> Env
unionEnv env1 (Env env2 adts) =
    Env (env1 `union` env2) adts


evalBranch :: Val -> PatternBranch Pos -> EM Val
evalBranch v p = case p of
  PatternBranch pat exp -> do
      patenv <- matchPattern v pat
      local (unionEnv patenv) (eval exp)



matchPattern :: Val -> Pattern Pos -> EM ValEnv
matchPattern v p = case p of
  PatternEmptyList _-> (do
        let (VADT name _) = v

        unless (name == "Empty")
          (throwError $ "EmptyList Pattern not matched "  ++ show v)
        return Data.Map.empty
        )
  PatternConsList _ pat1 pat2 -> (do

        let (VADT name [v1', v2']) =  v
        v1 <- normal v1'
        v2 <- normal v2'

        unless (name == "Cons")
          (
            throwError $ "ConsList Pattern not matched " ++ show v
            )

        env1 <- matchPattern v1 pat1
        env2 <- matchPattern v2 pat2


        return $ Data.Map.union env1 env2
        )

  PatternConstr _ s pats -> do
    let (VADT name vs) = v
    unless (s == name)
      (throwError $ "Constructors don't match " ++ name ++ " vs." ++ s)

    vs' <- mapM normal vs

    envs <- zipWithM matchPattern vs' pats

    let env' = unions envs

    return env'

    -- throwError "Match Pattern not yet implemented"

  PatternIdent _ s -> (do
          -- l <- newlock

          -- (mem, n, adts) <- get
 
          -- put (insert l v mem, n, adts)

          return $ Data.Map.fromList [(s, v)]
        )




evalNorm :: Exp Pos -> EM Val
evalNorm exp = do
    val <- eval exp
    normal val

