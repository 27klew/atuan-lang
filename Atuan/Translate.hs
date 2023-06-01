
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Atuan.Translate where

import Atuan.AlgorithmW (Exp(..), Lit(..), OpUn (OpNeg, OpNot), OpBin (..), MulOp (..), RelOp (..), AddOp(..), TypeEnv (..), Type (..), Scheme, generalize, PatternBranch (..), Pattern (..))

import qualified Atuan.Abs as A (Program'(..), Top' (TopDef, TopType), Ident (Ident), Def' (DefinitionT), Expr' (..), BoolLiteral (BoolLiteral), Lambda' (..), Val' (..), MulOp, MulOp' (Times, Div, Mod), RelOp' (..), AddOp', OTIdent' (..), TIdent' (..), AddOp'(..), Constr', TypeAnnot' (..), Type' (..), PatternBranch', Pattern'(..), ListPattern' (..), Field')
import Atuan.Abs (BoolLiteral, MulOp, Constr'(..), PatternBranch' (..), Field' (..), HasPosition(..), BNFC'Position)

import Atuan.CollectTypes (ADTs(..))
import qualified Data.Map (toList, empty, fromList)
import Data.Char (isLower)

type Expected a = Either String a

class Translatable a where
    translate :: a -> Expected (Exp Pos)


type Pos = BNFC'Position

iname :: A.OTIdent' a -> String
iname (A.OptionallyTypedIdentifier a (A.TypedIdentifier a2 (A.Ident n) _)) = n
iname (A.SkippedTypeIdentifier a (A.Ident n)) = n


translateDef :: A.Def' Pos -> Expected (Exp Pos, String)
translateDef (A.DefinitionT a (A.Ident i) ids t exp) = do
    exp' <- translate exp
    let ids' = map iname ids
    case ids' of
        [] -> return (exp', i)
        iss -> return (app, i)
            where app = foldr (EAbs a) exp' iss

instance Translatable (A.Program' Pos) where
    translate (A.ProgramText a []) =
        return $ EVar a "main"
    translate (A.ProgramText a1 (x:xs)) = case x of
      A.TopDef a def -> (do
        (exp', name) <- translateDef def
        inner <- translate (A.ProgramText a1 xs)
        return $ ELetRec  a1 name exp' inner
        )
      A.TopType a td -> error "Type definitions should not be translated."

instance Translatable A.BoolLiteral where
    translate (A.BoolLiteral b) =
        return $ ELit Nothing (LBool Nothing b') where
            b' = b == "True"

instance Translatable (A.Lambda' Pos) where
  translate :: A.Lambda' Pos -> Expected (Exp Pos)
  translate (A.AnonymousFunction a ids t exp) = do
    (exp', _) <- translateDef (A.DefinitionT a (A.Ident "__anonymous__") ids t exp)
    return exp'

instance Translatable (A.Val' Pos) where
  translate :: A.Val' Pos -> Expected (Exp Pos)
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



instance Translatable (A.Expr' Pos) where
  translate :: A.Expr' Pos -> Expected (Exp Pos)
  translate (A.EVar a (A.Ident i)) = return $ EVar a i
  translate (A.ELitInt a n) = return $ ELit a (LInt a n)
  translate (A.ELitBool a b) = translate b
  translate (A.ELambda a l) = translate l
  translate (A.ELitList a vals) = do
        vals' <- mapM translate vals
        return $ ELit a (LList a vals')

  translate (A.EApp a e1 []) = translate e1

  translate (A.EApp a e1 xs) = do
        e1' <- translate e1
            -- x' = translate x
        xs' <- mapM translate xs
        return $ foldl (EApp a) e1' xs'

  translate (A.Neg a exp) = do
       exp' <- translate exp
       return $ EUnOp a OpNeg exp'

  translate (A.Not a exp) = do
       exp' <- translate exp
       return $ EUnOp a OpNot exp'


  translate (A.EMul a exp1 op exp2) = do
      exp1' <- translate exp1
      exp2' <- translate exp2
      let op' = translateOp op
      return $ EBinOp a exp1' op' exp2'


  translate (A.EAdd a exp1 op exp2) = do
      exp1' <- translate exp1
      exp2' <- translate exp2
      let op' = translateOp op
      return $ EBinOp a  exp1' op' exp2'
  translate (A.ERel a exp1 op exp2) = do
    exp1' <- translate exp1
    exp2' <- translate exp2
    let op' = translateOp op
    return $ EBinOp a  exp1' op' exp2'
  translate (A.EAnd a exp1 exp2) = do
    exp1' <- translate exp1
    exp2' <- translate exp2
    return $ EBinOp a exp1' OpAnd exp2'
  translate (A.EOr a exp1 exp2) = do
    exp1' <- translate exp1
    exp2' <- translate exp2
    return $ EBinOp a exp1' OpOr exp2'

  translate (A.EIf a exp exp1 exp2) = do
    exp' <- translate exp
    exp1' <- translate exp1
    exp2' <- translate exp2
    return $ EIf a exp' exp1' exp2'
  translate (A.ELet a def exp) = do
    (def', name) <- translateDef def
    exp' <- translate exp
    return $ ELet a name def' exp'
  translate (A.ELetRec a def exp) = do
      (def', name) <- translateDef def
      exp' <- translate exp
      return $ ELetRec a name def' exp'

  translate (A.EMatch a (A.Ident i) pbs) = do
    branches <- mapM translateBranch pbs
    return $ EMatch a i branches
  translate (A.ConsLit a x xs) =
     translate (A.EApp a (A.EVar a (A.Ident "Cons")) [x, xs])


translateBranch :: A.PatternBranch' Pos -> Expected (PatternBranch Pos)
translateBranch (BranchPattern a pat ex) = do
    pat' <- translatePattern pat
    ex' <- translate ex
    return $ PatternBranch pat' ex'


translatePatternField :: A.Field' Pos -> Expected (Pattern Pos)
translatePatternField field = case field of
  ConstrField a pat -> translatePattern pat
  ConstrFieldIdent a (A.Ident i) -> return $ PatternIdent a i


isStringLower :: String -> Bool
isStringLower s =
  let c = head s in
    isLower c


translatePattern  :: A.Pattern' Pos -> Expected (Pattern Pos)
translatePattern pat = case pat of
  A.PatternLiteral a lit -> Left "Literal Patterns are not supported yet."
  A.PatternConstr a (A.Ident i) fis ->
    if isStringLower i 
      then 
        (
          if null fis 
            then 
              Right (PatternIdent a i) 
            else 
              Left "lowercase name should be an ident, not constr") 
      else (do
        fis' <- mapM translatePatternField fis
        return $ PatternConstr a i fis'
      )


    -- 
  A.PatternList a lp ->
      case lp of
        A.PatternEmptyList a -> return $ PatternEmptyList a
        A.PatternConsList a p1 p2 -> (do
          p1' <- translatePattern p1
          p2' <- translatePattern p2
          return $ PatternConsList a p1' p2'
          )


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
