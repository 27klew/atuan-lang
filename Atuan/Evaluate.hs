module Atuan.Evaluate where


import Atuan.AlgorithmW (Exp(..), Lit (..), OpBin (..), MulOp (..), AddOp (..), RelOp (..), OpUn (..))

import Data.Map(Map(..), member, lookup, union, fromList, insert, empty)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local), runReader)
import Control.Monad.State (StateT (runStateT), MonadState (..), evalStateT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Except (ExceptT, throwError, unless, runExceptT)


type Env =  Data.Map.Map String Loc


data Val = VInt Integer
    | VBool Bool
    | VFun String Env Exp
    | VADT String [Val]
    | VExp Exp Env deriving (Eq, Ord, Show)


type Loc = Int

type State = (Mem, Loc)

type Mem = Data.Map.Map Loc Val

type Expected a = ExceptT String a

type EM a = ReaderT Env (StateT State (Expected Identity)) a


testEval :: Exp -> Either String Val
testEval exp =
    let x = do
        exp' <- eval exp
        normal exp'
    in
    let y = runReaderT x Data.Map.empty in
    let z = evalStateT y (Data.Map.empty, 0) in
    let w = runExceptT z in
    let q = runIdentity w in
        q
    -- let v = runReaderT Data.Map.empty (runStateT (Data.Map.empty, 0) (runExceptT (eval exp))) in
        -- v



getMem :: EM Mem
getMem = do
    (mem, _) <- get
    return mem


getLoc :: String -> EM Loc
getLoc s = do
    env <- ask
    case  Data.Map.lookup s env of
      Nothing -> throwError $ "Unknown variable: " ++ s
      Just n -> return n


newlock :: EM Loc
newlock = do
    (mem, n) <- get
    put (mem, n+1)
    return n


-- setNew :: String -> Val -> EM Loc
-- setNew s v = do
--     l <- newlock
--     (mem, nnnn) <- get
--     insert l n 


-- TODO --- this should probably have it's  own value
normal :: Val -> EM Val
normal v = case v of
  VExp exp env -> do
        v' <- local (const env)(eval exp)
        normal v'

  v' -> return v'



askVal :: String -> EM  Val
askVal s = do
    loc <- getLoc s
    mem <- getMem

    case Data.Map.lookup loc mem of
      Nothing -> throwError $ "Unknown location: " ++ show loc
      Just val -> return val





eval :: Exp -> EM Val
eval exp = case exp of
  EVar s -> do
    askVal s

  ELit lit -> case lit of
    LInt n -> return $ VInt n
    LBool b -> return $ VBool b
    LList exps -> throwError "TODO. Should it exist? Maybe translate it to ADT."


  EApp exp' exp2 -> do
    f <- evalNorm exp'
    x <- eval exp2

    case f of
      VFun s map exp3 -> do
          l <- newlock
          (mem, n) <- get
          put (insert l x mem, n)
          local (union (fromList [(s, l)] `union` map)) (eval exp3)

      _ -> throwError "Runtime Error: EApp on non-function argument. This should not happen TODO"


  EAbs s exp' -> do
    env <- ask
    return $ VFun s env exp'


  ELet s exp' exp2 -> do
    v <- eval exp'
    l <- newlock
    (mem, n) <- get
    put (insert l v mem, n)


    local (union (fromList [(s, l)])) (eval exp2)

    -- throwError "Not yet implemented"


  ELetRec s exp' exp2 -> do
    l <- newlock
    (mem, n) <- get
    env <- ask
    let v = VExp exp' env
    put (insert l v mem, n)
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


  EMatch s pbs -> throwError "Not yet implemented"




evalNorm :: Exp -> EM Val
evalNorm exp = do
    val <- eval exp
    normal val

