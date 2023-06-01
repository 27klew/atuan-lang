module Atuan.Evaluate where


import Atuan.AlgorithmW (Exp(..), Lit (..), OpBin (..), MulOp (..), AddOp (..), RelOp (..), OpUn (..), PatternBranch (..), Pattern (..))

import Data.Map(Map(..), member, lookup, union, fromList, insert, empty, toList, unions)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local), runReader)
import Control.Monad.State (StateT (runStateT), MonadState (..), evalStateT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Except (ExceptT, throwError, unless, runExceptT, MonadError (catchError))
import Atuan.CollectTypes (ADTs (..))
import qualified Atuan.Abs
import Atuan.Abs (Constr' (..), HasPosition)
import Atuan.Translate (iname, itname, Pos)
import Debug.Trace
import Control.Monad (zipWithM)


type Env =  Data.Map.Map String Loc


data Val' a = VInt Integer
    | VBool Bool
    | VFun String Env (Exp a)
    | VADT String [Val' a]
    | VExp (Exp a) Env deriving (Eq, Ord, Show)

type Val = Val' Pos

type Loc = Int

type State = (Mem, Loc, ADTs Pos)

type Mem = Data.Map.Map Loc (Val)

type Expected a = ExceptT String a

type EM a = ReaderT Env (StateT State (Expected Identity)) a


-- constrToName :: Constr' a -> String
-- constrToName con = case con of { DataConstructor a (Atuan.Abs.Ident id) ta -> id}

setupEnv :: ADTs Atuan.Abs.BNFC'Position -> (Env, State)
setupEnv adts =
    let constr = from_constr adts in
        let cs = Data.Map.toList constr
            in let names = map (itname . fst) cs in
                foldr (flip f) (Data.Map.empty, (Data.Map.empty, 0, adts)) names


f :: (Env, State) -> String -> (Env, State)
f (env, (mem, loc, adts)) name =
    (insert name loc env, (insert loc (VADT name []) mem, loc+1, adts))


testEval :: ADTs  Atuan.Abs.BNFC'Position -> Exp Pos -> Either String (Val)
testEval adts exp =
    let x = do
        exp' <- eval exp :: EM (Val)
        normal exp'
    in
    let (env, state) = setupEnv adts in
    let y = runReaderT x env in
    let z = evalStateT y state in
    let w = runExceptT z in
    let q = runIdentity w in
        q
    -- let v = runReaderT Data.Map.empty (runStateT (Data.Map.empty, 0) (runExceptT (eval exp))) in
        -- v



getMem :: EM Mem
getMem = do
    (mem, _, _) <- get
    return mem


getLoc :: String -> EM Loc
getLoc s = do
    env <- ask
    case  Data.Map.lookup s env of
      Nothing -> throwError $ "Unknown variable: " ++ s
      Just n -> return n


newlock :: EM Loc
newlock = do
    (mem, n, a) <- get
    put (mem, n+1, a)
    return n


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



askVal' :: String -> EM  (Val)
askVal' s = do
    loc <- getLoc s
    mem <- getMem

    case Data.Map.lookup loc mem of
      Nothing -> throwError $ "Unknown location: " ++ show loc
      Just val -> return val


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
          l <- newlock
          (mem, n, a) <- get
          put (insert l x mem, n, a)
          local (union (fromList [(s, l)] `union` map)) (eval exp3)

      VADT s vs ->
        return $ VADT s (vs ++ [x])
      _ -> throwError "EApp on non-function argument. This should not happen"


  EAbs pos s exp' -> do
    env <- ask
    return $ VFun s env exp'


  ELet pos s exp' exp2 -> do
    v <- eval exp'
    l <- newlock
    (mem, n, a) <- get
    put (insert l v mem, n, a)


    local (union (fromList [(s, l)])) (eval exp2)


  ELetRec pos s exp' exp2 -> do
    l <- newlock
    (mem, n, a) <- get
    env <- ask
    let v = VExp exp' (fromList [(s, l)] `union` env )
    put (insert l v mem, n, a)

    local (union (fromList [(s, l)])) (eval exp2)

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



evalBranch :: Val -> PatternBranch Pos -> EM (Val)
evalBranch v p = case p of
  PatternBranch pat exp -> do
      patenv <- matchPattern v pat
      local (Data.Map.union patenv) (eval exp)



matchPattern :: Val -> Pattern Pos -> EM Env
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
          l <- newlock

          (mem, n, adts) <- get

          put (insert l v mem, n, adts)

          return $ Data.Map.fromList [(s, l)]
        )




evalNorm :: Exp Pos -> EM (Val)
evalNorm exp = do
    val <- eval exp
    normal val

