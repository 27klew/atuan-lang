-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The abstract syntax of language Atuan.

module AbsAtuan where

import Prelude (Integer, String, Bool)
import qualified Prelude as C
  ( Eq, Ord, Show, Read
  , Functor, Foldable, Traversable
  , Int, Maybe(..)
  )
import qualified Data.String

type Program = Program' BNFC'Position
data Program' a = ProgramText a [Top' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Top = Top' BNFC'Position
data Top' a = TopDef a (Def' a) | TopType a (TypeDef' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Def = Def' BNFC'Position
data Def' a = DefinitionT a [Ident] (OptTypeAnnot' a) (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type TypeDef = TypeDef' BNFC'Position
data TypeDef' a = TypeDefinition a Ident [TVar' a] [Constr' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type TVar = TVar' BNFC'Position
data TVar' a = TypeVariable a Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Constr = Constr' BNFC'Position
data Constr' a = DataConstructor a Ident (TypeAnnot' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type TypeAnnot = TypeAnnot' BNFC'Position
data TypeAnnot' a = TypeAnnotation a (Type' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type OptTypeAnnot = OptTypeAnnot' BNFC'Position
data OptTypeAnnot' a = OptionalTypeAnnotation a (TypeAnnot' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Block = Block' BNFC'Position
data Block' a = CurlyBlock a (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Lambda = Lambda' BNFC'Position
data Lambda' a
    = AnonymousFunction a [Ident] (OptTypeAnnot' a) (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Expr = Expr' BNFC'Position
data Expr' a
    = EVar a Ident
    | ELitInt a Integer
    | ELitBool a BoolLiteral
    | ELambda a (Lambda' a)
    | ELitList a [Val' a]
    | EApp a (Expr' a) [Expr' a]
    | Neg a (Expr' a)
    | Not a (Expr' a)
    | EMul a (Expr' a) (MulOp' a) (Expr' a)
    | EAdd a (Expr' a) (AddOp' a) (Expr' a)
    | ERel a (Expr' a) (RelOp' a) (Expr' a)
    | EAnd a (Expr' a) (Expr' a)
    | EOr a (Expr' a) (Expr' a)
    | EMatch a Ident [PatternBranch' a]
    | EIf a (Expr' a) (Expr' a) (Expr' a)
    | ELet a (Def' a) (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Val = Val' BNFC'Position
data Val' a = ValList a (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type PatternBranch = PatternBranch' BNFC'Position
data PatternBranch' a = BranchPattern a (Pattern' a) (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type AddOp = AddOp' BNFC'Position
data AddOp' a = Plus a | Minus a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type MulOp = MulOp' BNFC'Position
data MulOp' a = Times a | Div a | Mod a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type RelOp = RelOp' BNFC'Position
data RelOp' a = LTH a | LE a | GTH a | GE a | EQU a | NE a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type ListPattern = ListPattern' BNFC'Position
data ListPattern' a
    = PatternEmptyList a | PatternConsList a (Pattern' a) (Pattern' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Pattern = Pattern' BNFC'Position
data Pattern' a
    = PatternIdent a Ident
    | PatternConstr a Ident [Field' a]
    | PatternLiteral a (Literal' a)
    | PatternList a (ListPattern' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Field = Field' BNFC'Position
data Field' a = ConstrField a (Pattern' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Literal = Literal' BNFC'Position
data Literal' a
    = IntLit a Integer
    | BoolLit a BoolLiteral
    | LiteralList a [Literal' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Type = Type' BNFC'Position
data Type' a
    = TypeInt a
    | TypeBool a
    | TypeList a (Type' a)
    | TypeIdent a Ident
    | TypeApp a Ident [Type' a]
    | TypeFunc a (Type' a) (Type' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype BoolLiteral = BoolLiteral String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

-- | Start position (line, column) of something.

type BNFC'Position = C.Maybe (C.Int, C.Int)

pattern BNFC'NoPosition :: BNFC'Position
pattern BNFC'NoPosition = C.Nothing

pattern BNFC'Position :: C.Int -> C.Int -> BNFC'Position
pattern BNFC'Position line col = C.Just (line, col)

-- | Get the start position of something.

class HasPosition a where
  hasPosition :: a -> BNFC'Position

instance HasPosition Program where
  hasPosition = \case
    ProgramText p _ -> p

instance HasPosition Top where
  hasPosition = \case
    TopDef p _ -> p
    TopType p _ -> p

instance HasPosition Def where
  hasPosition = \case
    DefinitionT p _ _ _ -> p

instance HasPosition TypeDef where
  hasPosition = \case
    TypeDefinition p _ _ _ -> p

instance HasPosition TVar where
  hasPosition = \case
    TypeVariable p _ -> p

instance HasPosition Constr where
  hasPosition = \case
    DataConstructor p _ _ -> p

instance HasPosition TypeAnnot where
  hasPosition = \case
    TypeAnnotation p _ -> p

instance HasPosition OptTypeAnnot where
  hasPosition = \case
    OptionalTypeAnnotation p _ -> p

instance HasPosition Block where
  hasPosition = \case
    CurlyBlock p _ -> p

instance HasPosition Lambda where
  hasPosition = \case
    AnonymousFunction p _ _ _ -> p

instance HasPosition Expr where
  hasPosition = \case
    EVar p _ -> p
    ELitInt p _ -> p
    ELitBool p _ -> p
    ELambda p _ -> p
    ELitList p _ -> p
    EApp p _ _ -> p
    Neg p _ -> p
    Not p _ -> p
    EMul p _ _ _ -> p
    EAdd p _ _ _ -> p
    ERel p _ _ _ -> p
    EAnd p _ _ -> p
    EOr p _ _ -> p
    EMatch p _ _ -> p
    EIf p _ _ _ -> p
    ELet p _ _ -> p

instance HasPosition Val where
  hasPosition = \case
    ValList p _ -> p

instance HasPosition PatternBranch where
  hasPosition = \case
    BranchPattern p _ _ -> p

instance HasPosition AddOp where
  hasPosition = \case
    Plus p -> p
    Minus p -> p

instance HasPosition MulOp where
  hasPosition = \case
    Times p -> p
    Div p -> p
    Mod p -> p

instance HasPosition RelOp where
  hasPosition = \case
    LTH p -> p
    LE p -> p
    GTH p -> p
    GE p -> p
    EQU p -> p
    NE p -> p

instance HasPosition ListPattern where
  hasPosition = \case
    PatternEmptyList p -> p
    PatternConsList p _ _ -> p

instance HasPosition Pattern where
  hasPosition = \case
    PatternIdent p _ -> p
    PatternConstr p _ _ -> p
    PatternLiteral p _ -> p
    PatternList p _ -> p

instance HasPosition Field where
  hasPosition = \case
    ConstrField p _ -> p

instance HasPosition Literal where
  hasPosition = \case
    IntLit p _ -> p
    BoolLit p _ -> p
    LiteralList p _ -> p

instance HasPosition Type where
  hasPosition = \case
    TypeInt p -> p
    TypeBool p -> p
    TypeList p _ -> p
    TypeIdent p _ -> p
    TypeApp p _ _ -> p
    TypeFunc p _ _ -> p




-- data Value = IntValue C.Int | BoolValue Bool 


-- class Evaluable a where
--   eval :: a -> Value

-- instance Evaluable Top where
--   eval = \case
    
