module Atuan.Evaluate where


import Atuan.AlgorithmW (Exp(..), Lit (..), OpBin (..), MulOp (..), AddOp (..), RelOp (..), OpUn (..), PatternBranch (..), Pattern (..))

import Data.Map(Map(..), member, lookup, union, fromList, insert, empty, toList)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local), runReader)
import Control.Monad.State (StateT (runStateT), MonadState (..), evalStateT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Except (ExceptT, throwError, unless, runExceptT, MonadError (catchError))
import Atuan.CollectTypes (ADTs (..))
import qualified Atuan.Abs
import Atuan.Abs (Constr' (..))
import Atuan.Translate (iname, itname)


type Env =  Data.Map.Map String Loc


data Val = VInt Integer
    | VBool Bool
    | VFun String Env Exp
    | VADT String [Val]
    | VExp Exp Env deriving (Eq, Ord, Show)


type Loc = Int

type State b = (Mem, Loc, ADTs  Atuan.Abs.BNFC'Position)

type Mem = Data.Map.Map Loc Val

type Expected a = ExceptT String a

type EM a b = ReaderT Env (StateT (State b)  (Expected Identity)) a


-- constrToName :: Constr' a -> String
-- constrToName con = case con of { DataConstructor a (Atuan.Abs.Ident id) ta -> id}

setupEnv :: ADTs Atuan.Abs.BNFC'Position -> (Env, State Atuan.Abs.BNFC'Position)
setupEnv adts =
    let constr = from_constr adts in
        let cs = Data.Map.toList constr
            in let names = map (itname . fst) cs in
                foldr (flip f) (Data.Map.empty, (Data.Map.empty, 0, adts)) names


f :: (Env, State Atuan.Abs.BNFC'Position) -> String -> (Env, State Atuan.Abs.BNFC'Position)
f (env, (mem, loc, adts)) name =
    (insert name loc env, (insert loc (VADT name []) mem, loc+1, adts))


testEval :: ADTs  Atuan.Abs.BNFC'Position -> Exp -> Either String Val
testEval adts exp =
    let x = do
        exp' <- eval exp :: EM Val  Atuan.Abs.BNFC'Position
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



getMem :: EM Mem b
getMem = do
    (mem, _, _) <- get
    return mem


getLoc :: String -> EM Loc b
getLoc s = do
    env <- ask
    case  Data.Map.lookup s env of
      Nothing -> throwError $ "Unknown variable: " ++ s
      Just n -> return n


newlock :: EM Loc b
newlock = do
    (mem, n, a) <- get
    put (mem, n+1, a)
    return n


-- setNew :: String -> Val -> EM Loc
-- setNew s v = do
--     l <- newlock
--     (mem, nnnn) <- get
--     insert l n 


-- TODO --- this should probably have it's  own value
normal :: Val -> EM Val b
normal v = case v of
  VExp exp env -> do
        v' <- local (const env)(eval exp)
        normal v'

  v' -> return v'



askVal :: String -> EM  Val b
askVal s = do
    loc <- getLoc s
    mem <- getMem

    case Data.Map.lookup loc mem of
      Nothing -> throwError $ "Unknown location: " ++ show loc
      Just val -> return val


-- setup :: ADTs b -> EM () b
-- setup adts = do
--     let x = (from_name adts) 
--     throwError "aaa"


eval :: Exp -> EM Val b
eval exp = case exp of
  EVar s -> do
    askVal s

  ELit lit -> case lit of
    LInt n -> return $ VInt n
    LBool b -> return $ VBool b
    LList exps -> do
        vals <- mapM eval exps
        return (foldr (\x a-> VADT "Cons" [x, a]) (VADT "Empty" [])  vals)


  EApp exp' exp2 -> do
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

      _ -> throwError "Runtime Error: EApp on non-function argument. This should not happen TODO"


  EAbs s exp' -> do
    env <- ask
    return $ VFun s env exp'


  ELet s exp' exp2 -> do
    v <- eval exp'
    l <- newlock
    (mem, n, a) <- get
    put (insert l v mem, n, a)


    local (union (fromList [(s, l)])) (eval exp2)

    -- throwError "Not yet implemented"


  ELetRec s exp' exp2 -> do
    l <- newlock
    (mem, n, a) <- get
    env <- ask
    let v = VExp exp' env
    put (insert l v mem, n, a)
    -- TODO does it make sens? 

    local (union (fromList [(s, l)])) (eval exp2)


  EIf exp' exp2 exp3 -> do
    cond <- evalNorm exp'

    case cond of
      VBool b -> evalNorm (if b then exp2 else exp3)
      _ -> throwError "Incorrect type of value. this should not happen."


  EBinOp exp' ob exp2 -> do
    case ob of
      OpMul mo -> (do
        v1 <- evalNorm exp'
        v2 <- evalNorm exp2

        let VInt v1' = v1
        let VInt v2' = v2

        (case mo of
            Times -> return (VInt $ v1' * v2')
            Div -> if v2' == 0 then throwError "Error: Division by zero."
                    else return (VInt $ v1' `div` v2')
            Mod -> if v2' == 0 then throwError "Error: Modulo by zero."
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


  EUnOp ou exp' -> case ou of
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


  EMatch s pbs -> do
    v <- evalNorm (EVar s)
    evalBranches v pbs



evalBranches :: Val -> [PatternBranch] -> EM Val b
evalBranches v [] = throwError $ "Pattern match non-exhaustive on value " ++ show v
evalBranches v (p:ps) = do
      evalBranch v p `catchError` const (evalBranches v ps)



evalBranch :: Val -> PatternBranch -> EM Val b
evalBranch v p = case p of 
  PatternBranch pat exp -> do
      patenv <- matchPattern v pat
      local (Data.Map.union patenv) (eval exp) 
      


matchPattern :: Val -> Pattern -> EM Env b
matchPattern v p = case p of
  PatternEmptyList -> (do
        let (VADT name vs) = v 
        unless (name == "Empty")
          (throwError $ "EmptyList Pattern not matched "  ++ show v)
        return Data.Map.empty
        )
  PatternConsList pat1 pat2 -> (do
        
        let (VADT name [v1, v2]) = v 
        unless (name == "Cons")
          ( 
            throwError $ "ConsList Pattern not matched " ++ show v
            )
        
        env1 <- matchPattern v1 pat1
        env2 <- matchPattern v2 pat2

        return $ Data.Map.union env1 env2
        )
    
  PatternConstr s pats -> throwError "Match Pattern not yet implemented"
  
  PatternIdent s -> (do
          l <- newlock
          
          (mem, n, adts) <- get

          put (insert l v mem, n, adts)

          return $ Data.Map.fromList [(s, l)]
        )




evalNorm :: Exp -> EM Val b
evalNorm exp = do
    val <- eval exp
    normal val

