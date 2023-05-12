module Atuan.Evaluate where


import Atuan.AlgorithmW (Exp(..), Lit (..))

import Data.Map(Map(..), member, lookup, union, fromList, insert)
import Control.Monad.Reader (ReaderT, MonadReader (ask, local))
import Control.Monad.State (StateT, MonadState (..))
import Control.Monad.Identity (Identity)
import Control.Monad.Except (ExceptT, throwError, unless)



type Env =  Data.Map.Map String Loc


data Val = VInt Integer | VBool Bool | VFun String Env Exp | VADT String [Val]


type Loc = Int

type State = (Mem, Loc)

type Mem = Data.Map.Map Loc Val

type Expected a = ExceptT String a

type EM a = ReaderT Env (StateT State (Expected Identity)) a


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
    LList exps -> throwError "TODO. Should it exis? Maybe translate it to ADT."


  EApp exp' exp2 -> do
    f <- eval exp'
    x <- eval exp2

    case f of
      VFun s map exp3 -> do
          l <- newlock
          (mem, n) <- get
          put (insert l x mem, n)
          local (union (fromList [(s, l)] `union` map)) (eval exp3)

      _ -> throwError "Runtime Error: EApp on non-function argument. This should not happen TODO"


  EAbs s exp' -> throwError "Not yet implemented"

  ELet s exp' exp2 -> throwError "Not yet implemented"

  ELetRec s exp' exp2 -> throwError "Not yet implemented"

  EIf exp' exp2 exp3 -> throwError "Not yet implemented"

  EBinOp exp' ob exp2 -> throwError "Not yet implemented"

  EUnOp ou exp' -> throwError "Not yet implemented"

  EMatch s pbs -> throwError "Not yet implemented"




