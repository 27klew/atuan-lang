-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Atuan.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Atuan.Abs
import Atuan.Lex

}

%name pProgram_internal Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'           { PT _ (TS _ 1)          }
  '!='          { PT _ (TS _ 2)          }
  '%'           { PT _ (TS _ 3)          }
  '&&'          { PT _ (TS _ 4)          }
  '('           { PT _ (TS _ 5)          }
  '(lambda'     { PT _ (TS _ 6)          }
  ')'           { PT _ (TS _ 7)          }
  '*'           { PT _ (TS _ 8)          }
  '+'           { PT _ (TS _ 9)          }
  ','           { PT _ (TS _ 10)         }
  '-'           { PT _ (TS _ 11)         }
  '->'          { PT _ (TS _ 12)         }
  '/'           { PT _ (TS _ 13)         }
  ':'           { PT _ (TS _ 14)         }
  '::'          { PT _ (TS _ 15)         }
  ';'           { PT _ (TS _ 16)         }
  '<'           { PT _ (TS _ 17)         }
  '<='          { PT _ (TS _ 18)         }
  '='           { PT _ (TS _ 19)         }
  '=='          { PT _ (TS _ 20)         }
  '=>'          { PT _ (TS _ 21)         }
  '>'           { PT _ (TS _ 22)         }
  '>='          { PT _ (TS _ 23)         }
  '>>>'         { PT _ (TS _ 24)         }
  'Bool'        { PT _ (TS _ 25)         }
  'Int'         { PT _ (TS _ 26)         }
  '['           { PT _ (TS _ 27)         }
  '[]'          { PT _ (TS _ 28)         }
  ']'           { PT _ (TS _ 29)         }
  'data'        { PT _ (TS _ 30)         }
  'else'        { PT _ (TS _ 31)         }
  'if'          { PT _ (TS _ 32)         }
  'in'          { PT _ (TS _ 33)         }
  'let'         { PT _ (TS _ 34)         }
  'match'       { PT _ (TS _ 35)         }
  'then'        { PT _ (TS _ 36)         }
  'where'       { PT _ (TS _ 37)         }
  'with'        { PT _ (TS _ 38)         }
  '{'           { PT _ (TS _ 39)         }
  '||'          { PT _ (TS _ 40)         }
  '}'           { PT _ (TS _ 41)         }
  L_Ident       { PT _ (TV _)            }
  L_integ       { PT _ (TI _)            }
  L_BoolLiteral { PT _ (T_BoolLiteral _) }

%%

Ident :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Ident) }
Ident  : L_Ident { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.Ident (tokenText $1)) }

Integer :: { (Atuan.Abs.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

BoolLiteral :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.BoolLiteral) }
BoolLiteral  : L_BoolLiteral { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.BoolLiteral (tokenText $1)) }

Program :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Program) }
Program
  : ListTop { (fst $1, Atuan.Abs.ProgramText (fst $1) (snd $1)) }

Top :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Top) }
Top
  : Def { (fst $1, Atuan.Abs.TopDef (fst $1) (snd $1)) }
  | TypeDef { (fst $1, Atuan.Abs.TopType (fst $1) (snd $1)) }

Def :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Def) }
Def
  : Ident ListOTIdent OptTypeAnnot '=' Expr { (fst $1, Atuan.Abs.DefinitionT (fst $1) (snd $1) (snd $2) (snd $3) (snd $5)) }

TIdent :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.TIdent) }
TIdent
  : '(' Ident TypeAnnot ')' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.TypedIdentifier (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3)) }

OTIdent :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.OTIdent) }
OTIdent
  : TIdent { (fst $1, Atuan.Abs.OptionallyTypedIdentifier (fst $1) (snd $1)) }
  | Ident { (fst $1, Atuan.Abs.SkippedTypeIdentifier (fst $1) (snd $1)) }

TypeDef :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.TypeDef) }
TypeDef
  : 'data' Ident ListTVar 'where' ListConstr { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.TypeDefinition (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3) (snd $5)) }

TVar :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.TVar) }
TVar : Ident { (fst $1, Atuan.Abs.TypeVariable (fst $1) (snd $1)) }

ListTVar :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.TVar]) }
ListTVar
  : {- empty -} { (Atuan.Abs.BNFC'NoPosition, []) }
  | TVar ListTVar { (fst $1, (:) (snd $1) (snd $2)) }

Constr :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Constr) }
Constr
  : Ident TypeAnnot { (fst $1, Atuan.Abs.DataConstructor (fst $1) (snd $1) (snd $2)) }

ListConstr :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.Constr]) }
ListConstr
  : Constr { (fst $1, (:[]) (snd $1)) }
  | Constr ListConstr { (fst $1, (:) (snd $1) (snd $2)) }

TypeAnnot :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.TypeAnnot) }
TypeAnnot
  : '::' Type { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.TypeAnnotation (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

OptTypeAnnot :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.OptTypeAnnot) }
OptTypeAnnot
  : TypeAnnot { (fst $1, Atuan.Abs.OptionalTypeAnnotation (fst $1) (snd $1)) }
  | {- empty -} { (Atuan.Abs.BNFC'NoPosition, Atuan.Abs.SkippedTypeAnnotation Atuan.Abs.BNFC'NoPosition) }

ListTop :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.Top]) }
ListTop
  : Top ';' { (fst $1, (:[]) (snd $1)) }
  | Top ';' ListTop { (fst $1, (:) (snd $1) (snd $3)) }

ListIdent :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.Ident]) }
ListIdent
  : Ident { (fst $1, (:[]) (snd $1)) }
  | Ident ListIdent { (fst $1, (:) (snd $1) (snd $2)) }

ListOTIdent :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.OTIdent]) }
ListOTIdent
  : {- empty -} { (Atuan.Abs.BNFC'NoPosition, []) }
  | OTIdent ListOTIdent { (fst $1, (:) (snd $1) (snd $2)) }

Block :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Block) }
Block
  : '{' Expr '}' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.CurlyBlock (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

Lambda :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Lambda) }
Lambda
  : '(lambda' Ident ListOTIdent OptTypeAnnot '=>' Expr ')' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.AnonymousFunction (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3) (snd $4) (snd $6)) }

Expr8 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Expr) }
Expr8
  : Ident { (fst $1, Atuan.Abs.EVar (fst $1) (snd $1)) }
  | Integer { (fst $1, Atuan.Abs.ELitInt (fst $1) (snd $1)) }
  | BoolLiteral { (fst $1, Atuan.Abs.ELitBool (fst $1) (snd $1)) }
  | Lambda { (fst $1, Atuan.Abs.ELambda (fst $1) (snd $1)) }
  | '[' ListVal ']' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.ELitList (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | '(' Expr ')' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), (snd $2)) }

Val :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Val) }
Val : Expr8 { (fst $1, Atuan.Abs.ValList (fst $1) (snd $1)) }

ListVal :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.Val]) }
ListVal
  : {- empty -} { (Atuan.Abs.BNFC'NoPosition, []) }
  | Val { (fst $1, (:[]) (snd $1)) }
  | Val ',' ListVal { (fst $1, (:) (snd $1) (snd $3)) }

Expr7 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Expr) }
Expr7
  : Expr8 ListExpr8 { (fst $1, Atuan.Abs.EApp (fst $1) (snd $1) (snd $2)) }
  | Expr8 { (fst $1, (snd $1)) }

Expr6 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Expr) }
Expr6
  : '-' Expr7 { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.Neg (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | '!' Expr7 { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.Not (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr7 { (fst $1, (snd $1)) }

Expr5 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Expr) }
Expr5
  : Expr5 MulOp Expr6 { (fst $1, Atuan.Abs.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Expr) }
Expr4
  : Expr4 AddOp Expr5 { (fst $1, Atuan.Abs.EAdd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Expr) }
Expr3
  : Expr3 RelOp Expr4 { (fst $1, Atuan.Abs.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Expr) }
Expr2
  : Expr3 '&&' Expr2 { (fst $1, Atuan.Abs.EAnd (fst $1) (snd $1) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Expr) }
Expr1
  : Expr2 '||' Expr1 { (fst $1, Atuan.Abs.EOr (fst $1) (snd $1) (snd $3)) }
  | 'match' Ident 'with' ListPatternBranch { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.EMatch (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | 'if' Expr2 'then' Expr2 'else' Expr2 { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.EIf (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4) (snd $6)) }
  | 'let' Def 'in' Expr2 { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.ELet (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | Expr2 { (fst $1, (snd $1)) }

PatternBranch :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.PatternBranch) }
PatternBranch
  : '(' Pattern ')' '>>>' Expr2 { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.BranchPattern (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $5)) }

ListPatternBranch :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.PatternBranch]) }
ListPatternBranch
  : PatternBranch { (fst $1, (:[]) (snd $1)) }
  | PatternBranch ',' ListPatternBranch { (fst $1, (:) (snd $1) (snd $3)) }

Expr :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Expr) }
Expr : Expr1 { (fst $1, (snd $1)) }

ListExpr :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.Expr]) }
ListExpr
  : {- empty -} { (Atuan.Abs.BNFC'NoPosition, []) }
  | Expr ListExpr { (fst $1, (:) (snd $1) (snd $2)) }

ListExpr8 :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.Expr]) }
ListExpr8
  : Expr8 { (fst $1, (:[]) (snd $1)) }
  | Expr8 ListExpr8 { (fst $1, (:) (snd $1) (snd $2)) }

AddOp :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.AddOp) }
AddOp
  : '+' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.Plus (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.Minus (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.MulOp) }
MulOp
  : '*' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.Times (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.Div (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | '%' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.Mod (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.RelOp) }
RelOp
  : '<' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.LTH (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.LE (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.GTH (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.GE (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | '==' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.EQU (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | '!=' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.NE (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }

ListPattern :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.ListPattern) }
ListPattern
  : '[]' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.PatternEmptyList (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | Pattern4 ':' Pattern4 { (fst $1, Atuan.Abs.PatternConsList (fst $1) (snd $1) (snd $3)) }

Pattern5 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Pattern) }
Pattern5
  : Literal { (fst $1, Atuan.Abs.PatternLiteral (fst $1) (snd $1)) }
  | '(' Pattern ')' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), (snd $2)) }

Pattern4 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Pattern) }
Pattern4
  : Ident ListField { (fst $1, Atuan.Abs.PatternConstr (fst $1) (snd $1) (snd $2)) }
  | Pattern5 { (fst $1, (snd $1)) }

Pattern3 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Pattern) }
Pattern3
  : ListPattern { (fst $1, Atuan.Abs.PatternList (fst $1) (snd $1)) }
  | Pattern4 { (fst $1, (snd $1)) }

Pattern :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Pattern) }
Pattern : Pattern1 { (fst $1, (snd $1)) }

Pattern1 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Pattern) }
Pattern1 : Pattern2 { (fst $1, (snd $1)) }

Pattern2 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Pattern) }
Pattern2 : Pattern3 { (fst $1, (snd $1)) }

Field :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Field) }
Field
  : Pattern5 { (fst $1, Atuan.Abs.ConstrField (fst $1) (snd $1)) }
  | Ident { (fst $1, Atuan.Abs.ConstrFieldIdent (fst $1) (snd $1)) }

ListField :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.Field]) }
ListField
  : {- empty -} { (Atuan.Abs.BNFC'NoPosition, []) }
  | Field ListField { (fst $1, (:) (snd $1) (snd $2)) }

Literal :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Literal) }
Literal
  : Integer { (fst $1, Atuan.Abs.IntLit (fst $1) (snd $1)) }
  | BoolLiteral { (fst $1, Atuan.Abs.BoolLit (fst $1) (snd $1)) }
  | '[' ListLiteral ']' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.LiteralList (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListLiteral :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.Literal]) }
ListLiteral
  : {- empty -} { (Atuan.Abs.BNFC'NoPosition, []) }
  | Literal { (fst $1, (:[]) (snd $1)) }
  | Literal ',' ListLiteral { (fst $1, (:) (snd $1) (snd $3)) }

Type4 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Type) }
Type4
  : 'Int' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.TypeInt (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'Bool' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.TypeBool (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1))) }
  | '[' Type ']' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.TypeList (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Ident { (fst $1, Atuan.Abs.TypeIdent (fst $1) (snd $1)) }
  | '(' Type ')' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), (snd $2)) }

Type3 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Type) }
Type3
  : '(' Ident ListType4 ')' { (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1), Atuan.Abs.TypeApp (uncurry Atuan.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3)) }
  | Type4 { (fst $1, (snd $1)) }

ListType4 :: { (Atuan.Abs.BNFC'Position, [Atuan.Abs.Type]) }
ListType4
  : Type4 { (fst $1, (:[]) (snd $1)) }
  | Type4 ListType4 { (fst $1, (:) (snd $1) (snd $2)) }

Type1 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Type) }
Type1
  : Type2 '->' Type1 { (fst $1, Atuan.Abs.TypeFunc (fst $1) (snd $1) (snd $3)) }
  | Type2 { (fst $1, (snd $1)) }

Type :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Type) }
Type : Type1 { (fst $1, (snd $1)) }

Type2 :: { (Atuan.Abs.BNFC'Position, Atuan.Abs.Type) }
Type2 : Type3 { (fst $1, (snd $1)) }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err Atuan.Abs.Program
pProgram = fmap snd . pProgram_internal
}
