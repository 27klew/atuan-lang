-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Atuan.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Atuan.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Atuan.Abs.Ident -> Result
transIdent x = case x of
  Atuan.Abs.Ident string -> failure x

transBoolLiteral :: Atuan.Abs.BoolLiteral -> Result
transBoolLiteral x = case x of
  Atuan.Abs.BoolLiteral string -> failure x

transProgram :: Show a => Atuan.Abs.Program' a -> Result
transProgram x = case x of
  Atuan.Abs.ProgramText _ tops -> failure x

transTop :: Show a => Atuan.Abs.Top' a -> Result
transTop x = case x of
  Atuan.Abs.TopDef _ def -> failure x
  Atuan.Abs.TopType _ typedef -> failure x

transDef :: Show a => Atuan.Abs.Def' a -> Result
transDef x = case x of
  Atuan.Abs.DefinitionT _ ident otidents opttypeannot expr -> failure x

transTIdent :: Show a => Atuan.Abs.TIdent' a -> Result
transTIdent x = case x of
  Atuan.Abs.TypedIdentifier _ ident typeannot -> failure x

transOTIdent :: Show a => Atuan.Abs.OTIdent' a -> Result
transOTIdent x = case x of
  Atuan.Abs.OptionallyTypedIdentifier _ tident -> failure x
  Atuan.Abs.SkippedTypeIdentifier _ ident -> failure x

transTypeDef :: Show a => Atuan.Abs.TypeDef' a -> Result
transTypeDef x = case x of
  Atuan.Abs.TypeDefinition _ ident tvars constrs -> failure x

transTVar :: Show a => Atuan.Abs.TVar' a -> Result
transTVar x = case x of
  Atuan.Abs.TypeVariable _ ident -> failure x

transConstr :: Show a => Atuan.Abs.Constr' a -> Result
transConstr x = case x of
  Atuan.Abs.DataConstructor _ ident typeannot -> failure x

transTypeAnnot :: Show a => Atuan.Abs.TypeAnnot' a -> Result
transTypeAnnot x = case x of
  Atuan.Abs.TypeAnnotation _ type_ -> failure x

transOptTypeAnnot :: Show a => Atuan.Abs.OptTypeAnnot' a -> Result
transOptTypeAnnot x = case x of
  Atuan.Abs.OptionalTypeAnnotation _ typeannot -> failure x
  Atuan.Abs.SkippedTypeAnnotation _ -> failure x

transBlock :: Show a => Atuan.Abs.Block' a -> Result
transBlock x = case x of
  Atuan.Abs.CurlyBlock _ expr -> failure x

transLambda :: Show a => Atuan.Abs.Lambda' a -> Result
transLambda x = case x of
  Atuan.Abs.AnonymousFunction _ otidents opttypeannot expr -> failure x

transExpr :: Show a => Atuan.Abs.Expr' a -> Result
transExpr x = case x of
  Atuan.Abs.EVar _ ident -> failure x
  Atuan.Abs.ELitInt _ integer -> failure x
  Atuan.Abs.ELitBool _ boolliteral -> failure x
  Atuan.Abs.ELambda _ lambda -> failure x
  Atuan.Abs.ELitList _ vals -> failure x
  Atuan.Abs.EApp _ expr exprs -> failure x
  Atuan.Abs.Neg _ expr -> failure x
  Atuan.Abs.Not _ expr -> failure x
  Atuan.Abs.EMul _ expr1 mulop expr2 -> failure x
  Atuan.Abs.EAdd _ expr1 addop expr2 -> failure x
  Atuan.Abs.ERel _ expr1 relop expr2 -> failure x
  Atuan.Abs.EAnd _ expr1 expr2 -> failure x
  Atuan.Abs.EOr _ expr1 expr2 -> failure x
  Atuan.Abs.EMatch _ ident patternbranchs -> failure x
  Atuan.Abs.EIf _ expr1 expr2 expr3 -> failure x
  Atuan.Abs.ELet _ def expr -> failure x

transVal :: Show a => Atuan.Abs.Val' a -> Result
transVal x = case x of
  Atuan.Abs.ValList _ expr -> failure x

transPatternBranch :: Show a => Atuan.Abs.PatternBranch' a -> Result
transPatternBranch x = case x of
  Atuan.Abs.BranchPattern _ pattern_ expr -> failure x

transAddOp :: Show a => Atuan.Abs.AddOp' a -> Result
transAddOp x = case x of
  Atuan.Abs.Plus _ -> failure x
  Atuan.Abs.Minus _ -> failure x

transMulOp :: Show a => Atuan.Abs.MulOp' a -> Result
transMulOp x = case x of
  Atuan.Abs.Times _ -> failure x
  Atuan.Abs.Div _ -> failure x
  Atuan.Abs.Mod _ -> failure x

transRelOp :: Show a => Atuan.Abs.RelOp' a -> Result
transRelOp x = case x of
  Atuan.Abs.LTH _ -> failure x
  Atuan.Abs.LE _ -> failure x
  Atuan.Abs.GTH _ -> failure x
  Atuan.Abs.GE _ -> failure x
  Atuan.Abs.EQU _ -> failure x
  Atuan.Abs.NE _ -> failure x

transListPattern :: Show a => Atuan.Abs.ListPattern' a -> Result
transListPattern x = case x of
  Atuan.Abs.PatternEmptyList _ -> failure x
  Atuan.Abs.PatternConsList _ pattern_1 pattern_2 -> failure x

transPattern :: Show a => Atuan.Abs.Pattern' a -> Result
transPattern x = case x of
  Atuan.Abs.PatternLiteral _ literal -> failure x
  Atuan.Abs.PatternConstr _ ident fields -> failure x
  Atuan.Abs.PatternList _ listpattern -> failure x

transField :: Show a => Atuan.Abs.Field' a -> Result
transField x = case x of
  Atuan.Abs.ConstrField _ pattern_ -> failure x
  Atuan.Abs.ConstrFieldIdent _ ident -> failure x

transLiteral :: Show a => Atuan.Abs.Literal' a -> Result
transLiteral x = case x of
  Atuan.Abs.IntLit _ integer -> failure x
  Atuan.Abs.BoolLit _ boolliteral -> failure x
  Atuan.Abs.LiteralList _ literals -> failure x

transType :: Show a => Atuan.Abs.Type' a -> Result
transType x = case x of
  Atuan.Abs.TypeInt _ -> failure x
  Atuan.Abs.TypeBool _ -> failure x
  Atuan.Abs.TypeList _ type_ -> failure x
  Atuan.Abs.TypeIdent _ ident -> failure x
  Atuan.Abs.TypeApp _ ident types -> failure x
  Atuan.Abs.TypeFunc _ type_1 type_2 -> failure x
