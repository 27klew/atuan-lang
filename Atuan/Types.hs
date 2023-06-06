module Atuan.Types where
import Atuan.Abs (BNFC'Position)


type Pos = BNFC'Position

type Label = (Pos, Maybe Type)

data Exp a =  EVar a String
             |  ELit a (Lit a)
             |  EApp a (Exp a) (Exp a)
             |  EAbs a String (Exp a)
             |  ELet a String (Exp a) (Exp a)
             |  ELetRec a String (Exp a) (Exp a)
             |  EIf a (Exp a) (Exp a) (Exp a)
             |  EBinOp a (Exp a) OpBin (Exp a)
             |  EUnOp  a OpUn (Exp a)
             |  EMatch a String [PatternBranch a]

             deriving (Eq, Ord)


data Lit a    =  LInt a Integer
             |  LBool a Bool
             |  LList a [Exp a]
             deriving (Eq, Ord)

data Type    =  TVar String
             |  TInt
             |  TBool
             |  TFun Type Type
             |  ADT String [Type]
             deriving (Eq, Ord)

data Scheme  =  Scheme [String] Type

data PatternBranch a = PatternBranch (Pattern a) (Exp a) deriving (Eq, Ord)


data Pattern a =
    PatternEmptyList a
    | PatternConsList a (Pattern a) (Pattern a)
    -- | PatternLiteral Exp
    |  PatternConstr a String [Pattern a]
    |  PatternIdent a String
    deriving (Eq, Ord, Show)

data OpUn = OpNeg | OpNot deriving (Eq, Ord)
data OpBin = OpMul MulOp | OpAdd AddOp | OpRel RelOp | OpAnd | OpOr deriving (Eq, Ord)

data MulOp = Times | Div | Mod deriving (Eq, Ord)

data AddOp = Plus  | Minus deriving (Eq, Ord)

data RelOp = LTH  | LE  | GTH  | GE  | EQU | NE deriving (Eq, Ord)

