
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Atuan.Translate where

import Atuan.AlgorithmW (Exp(..), Lit(..), OpUn (OpNeg, OpNot), OpBin (..), MulOp (..), RelOp (..), AddOp(..), TypeEnv (..), Type (..), Scheme, generalize, PatternBranch (..), Pattern (..))

import qualified Atuan.Abs as A (Program'(..), Top' (TopDef, TopType), Ident (Ident), Def' (DefinitionT), Expr' (..), BoolLiteral (BoolLiteral), Lambda' (..), Val' (..), MulOp, MulOp' (Times, Div, Mod), RelOp' (..), AddOp', OTIdent' (..), TIdent' (..), AddOp'(..), Constr', TypeAnnot' (..), Type' (..), PatternBranch', Pattern'(..), ListPattern' (..), Field')
import Atuan.Abs (BoolLiteral, MulOp, Constr'(..), PatternBranch' (..), Field' (..))

import Atuan.CollectTypes (ADTs(..))
import qualified Data.Map (toList, empty, fromList)
import Data.Char (isLower)

class Translatable a where
    translate :: a -> Exp


iname :: A.OTIdent' a -> String
iname (A.OptionallyTypedIdentifier a (A.TypedIdentifier a2 (A.Ident n) _)) = n
iname (A.SkippedTypeIdentifier a (A.Ident n)) = n


translateDef :: A.Def' a -> (Exp, String)
translateDef (A.DefinitionT a (A.Ident i) ids t exp) =
    let exp' = translate exp in
    let ids' = map iname ids in
    case ids' of
        [] -> (exp', i)
        iss -> (app, i)
            where app = foldr EAbs exp' iss

instance Translatable (A.Program' a) where
    translate (A.ProgramText _ []) =
        EVar "main"
    translate (A.ProgramText a1 (x:xs)) = case x of
      A.TopDef a def ->
        let (exp', name) = translateDef def in
        ELetRec  name exp' (translate (A.ProgramText a1 xs))
      A.TopType a td -> error "Type definitions should not be translated."

instance Translatable A.BoolLiteral where
    translate (A.BoolLiteral b) =
        ELit (LBool b') where
            b' = b == "True"

instance Translatable (A.Lambda' a) where
  translate :: A.Lambda' a -> Exp
  translate (A.AnonymousFunction a ids t exp) = do
    let (exp', _) = translateDef (A.DefinitionT a (A.Ident "__anonymous__") ids t exp) in
      exp'

instance Translatable (A.Val' a) where
  translate :: A.Val' a -> Exp
  translate (A.ValList a expr) = translate expr


class TranslatableOp a where
    translateOp :: a -> OpBin


instance TranslatableOp (A.MulOp' a) where
    translateOp op = case op of
      A.Times a -> OpMul Times
      A.Div a -> OpMul Div
      A.Mod a -> OpMul Mod


instance TranslatableOp (A.RelOp' a) where
    translateOp op = case op of
      A.LTH a -> OpRel LTH
      A.LE a -> OpRel LE
      A.GTH a -> OpRel GTH
      A.GE a -> OpRel GE
      A.EQU a -> OpRel EQU
      A.NE a -> OpRel NE

instance TranslatableOp (A.AddOp' a) where
    translateOp op = case op of
      A.Plus a -> OpAdd Plus
      A.Minus a -> OpAdd Minus



instance Translatable (A.Expr' a) where
  translate :: A.Expr' a -> Exp
  translate (A.EVar a (A.Ident i)) = EVar i
  translate (A.ELitInt a n) = ELit (LInt n)
  translate (A.ELitBool a b) = translate b
  translate (A.ELambda a l) = translate l
  translate (A.ELitList a vals) =
         ELit (LList (map translate vals))

  translate (A.EApp a e1 []) = translate e1

  translate (A.EApp a e1 xs) =
        let e1' = translate e1
            -- x' = translate x
            xs' = map translate xs in
        foldl EApp e1' xs'

  translate (A.Neg a exp) = EUnOp OpNeg (translate exp)
  translate (A.Not a exp) = EUnOp OpNot (translate exp)
  translate (A.EMul a exp1 op exp2) = EBinOp (translate exp1) (translateOp op) (translate exp2)
  translate (A.EAdd a exp1 op exp2) = EBinOp (translate exp1) (translateOp op) (translate exp2)
  translate (A.ERel a exp1 op exp2) = EBinOp (translate exp1) (translateOp op) (translate exp2)
  translate (A.EAnd a exp1 exp2) = EBinOp (translate exp1) OpAnd (translate exp2)
  translate (A.EOr a exp1 exp2) = EBinOp (translate exp1) OpOr (translate exp2)

  translate (A.EIf a exp exp1 exp2) = EIf (translate exp) (translate exp1) (translate exp2)
  translate (A.ELet a def exp) =
    let (exp', name) = translateDef def in
        ELet name exp' (translate exp)

  translate (A.EMatch a (A.Ident i) pbs) = EMatch i (map translateBranch pbs)
  translate (A.ConsLit a x xs) =
     translate (A.EApp a (A.EVar a (A.Ident "cons")) [x, xs])

    -- TODO add match
    -- EMatch a Ident [PatternBranch' a]

translateBranch :: A.PatternBranch' a -> PatternBranch 
translateBranch br = case br of 
  BranchPattern a pat ex -> 
     let pat' = translatePattern pat in
     let ex' = translate ex in
      PatternBranch pat' ex' 


translatePatternField :: A.Field' a -> Pattern
translatePatternField field = case field of
  ConstrField a pat -> translatePattern pat
  ConstrFieldIdent a (A.Ident i) -> PatternIdent i


isStringLower :: String -> Bool 
isStringLower s = 
  let c = head s in
    isLower c


translatePattern  :: A.Pattern' a -> Pattern
translatePattern pat = case pat of 
  A.PatternLiteral a lit -> error "Literal Patterns are not supported yet."
  A.PatternConstr a (A.Ident i) fis -> case (isStringLower i) of
    False -> PatternConstr i  (map translatePatternField fis)
    True -> if null fis then PatternIdent i else error "lowercase name should be an ident, not constr"
    



    -- 
  A.PatternList a lp -> 
      case lp of 
        A.PatternEmptyList a -> PatternEmptyList
        A.PatternConsList a p1 p2 -> PatternConsList (translatePattern p1) (translatePattern p2) 



test_1 = (A.EApp () (A.EVar () (A.Ident "f")) [(A.EVar () (A.Ident "x"))])

test_2 = (A.EApp () (A.EVar () (A.Ident "f")) [(A.EVar () (A.Ident "x")), (A.EVar () (A.Ident "y"))])

test_3 = (A.EApp () (A.EVar () (A.Ident "f")) [(A.EVar () (A.Ident "x")), (A.EVar () (A.Ident "y")), (A.EVar () (A.Ident "z"))])



translateType2 :: A.Type' a -> Type
translateType2 t = case t of
  A.TypeInt a -> TInt
  A.TypeBool a -> TBool
  A.TypeList a ty -> ADT "List" [translateType2 ty]
  A.TypeIdent a id -> ADT (itname id) []
  A.TypeApp a id tys -> ADT (itname id) (map translateType2 tys)
  A.TypeFunc a ty ty' -> TFun (translateType2 ty) (translateType2 ty')
  A.TypeVar a id -> TVar (itname id)


translateType :: A.TypeAnnot' a -> Type
translateType t = case t of
    A.TypeAnnotation a ty -> translateType2 ty



translateConstr ::  A.Constr' a -> Scheme
translateConstr con = case con of
   DataConstructor a id ta ->
    let ta' = translateType ta in
    -- let name = itname id in
      generalize (TypeEnv Data.Map.empty) ta'


itname :: A.Ident -> String
itname (A.Ident i) = i

translateConstrs :: ADTs a -> TypeEnv
translateConstrs adt =
  let cs = from_constr adt in
  let cs' = Data.Map.toList cs in
  let names = map (itname . fst) cs' in
  let css = map (translateConstr . snd) cs' in
    TypeEnv $ Data.Map.fromList (zip names css)
    -- foldr Data.Map.insert Data.Map.empty (zip (map translateConstr cs') (map fst ))

-- >>> translate test_1
-- f x
-- >>> translate test_2
-- f x y

-- >>> translate test_3
-- f x y z
