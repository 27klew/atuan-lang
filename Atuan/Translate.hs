
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Atuan.Translate where

import Atuan.Types(Exp(..), Lit(..), OpUn (OpNeg, OpNot), OpBin (..), MulOp (..), RelOp (..), AddOp(..),  Type (..), Scheme, PatternBranch (..), Pattern (..), Label, Pos)
import Atuan.AlgorithmW ( generalize, TypeEnv (..),)

import qualified Atuan.Abs as A (Program'(..), Top' (TopDef, TopType), Ident (Ident), Def' (..), Expr' (..), BoolLiteral (BoolLiteral), Lambda' (..), Val' (..), MulOp, MulOp' (Times, Div, Mod), RelOp' (..), AddOp', OTIdent' (..), TIdent' (..), AddOp'(..), Constr', TypeAnnot' (..), Type' (..), PatternBranch', Pattern'(..), ListPattern' (..), Field')
import Atuan.Abs (BoolLiteral, MulOp, Constr'(..), PatternBranch' (..), Field' (..), HasPosition(..), BNFC'Position, OTIdent' (..), Ident, TIdent' (..), TypeAnnot' (..), Type' (..), OptTypeAnnot, OptTypeAnnot' (..), NTIdent, NTIdent' (..), TIdent)

import Atuan.CollectTypes (ADTs(..), identToVar')
import qualified Data.Map (toList, empty, fromList)
import Data.Char (isLower)
import Debug.Trace (trace)


type Expected a = Either String a

class Translatable a where
    translate :: a -> Expected (Exp Label)




iname :: A.OTIdent' a -> String
iname (A.OptionallyTypedIdentifier a (A.TypedIdentifier a2 (A.Ident n) _)) = n
iname (A.SkippedTypeIdentifier a (A.Ident n)) = n



makeType' :: Show a => Type' a -> Expected Type
makeType' ty' = do
  ty <- identToVar' ty'
  case ty of
    TypeInt a -> return TInt
    TypeBool a -> return TBool
    TypeList a ty' -> (do 
              ty'' <- makeType' ty'
              return $ ADT "List" [ty'']
            )
    TypeIdent a id -> return $ ADT (itname id) []
    TypeApp a id tys -> (do
                tys' <- mapM makeType' tys
                return $ ADT (itname id) tys'
             
             )
    TypeVar a id -> return $ TVar (itname id)
    TypeFunc a ty' ty_a -> (do 
          ty'' <- makeType' ty'
          ty_a' <- makeType' ty_a
          return $ TFun ty'' ty_a'
      )
makeType :: Show a => TypeAnnot' a -> Expected Type
makeType (TypeAnnotation a ty) = makeType' ty

makeLabelT :: Show a => TIdent' a -> Expected (Ident, Maybe Type)
makeLabelT (TypedIdentifier a id ta) = do
  ta' <- makeType ta
  return (id, Just ta')


makeEmptyLabel :: Show a => NTIdent' a -> Expected (Ident, Maybe Type)
makeEmptyLabel (UnTypedIndent a id ) = return (id, Nothing)
  -- OptionallyTypedIdentifier a ti -> makeLabelT ti
  -- SkippedTypeIdentifier a id -> return (id, Nothing)


makeLabel :: Show a => OTIdent' a -> Expected (Ident, Maybe Type)
makeLabel oti = case oti of
  OptionallyTypedIdentifier a ti -> makeLabelT ti
  SkippedTypeIdentifier a id -> return (id, Nothing)




makeLabelAnn :: Show a => TypeAnnot' a -> Expected (Maybe Type)
makeLabelAnn (TypeAnnotation a ty) = do
      ty' <- makeType' ty     
      return (Just ty')
  
  -- OptionalTypeAnnotation a ta -> (do
  --     ta' <- makeType ta
  --     return (Just $ ta')
  --   )
  -- SkippedTypeAnnotation a -> return Nothing


makeLabel' :: Show a => OptTypeAnnot' a -> Expected (Maybe Type)
makeLabel' opt = case opt of
  OptionalTypeAnnotation a ta -> (do
      ta' <- makeType ta
      return (Just ta')
    )
  SkippedTypeAnnotation a -> return Nothing


makeFunType :: [Maybe Type] -> Maybe Type
makeFunType [] = error "Incorrect usage of makeFunType"
makeFunType [t] = t
makeFunType (t:ts) = do
  t' <- t
  ts' <- makeFunType ts
  return $ TFun t' ts'


setTypeInLabel :: Label -> Maybe Type -> Label
setTypeInLabel (p, _) t = (p, t)


setTypeLabel :: Exp Label -> Maybe Type -> Exp Label
setTypeLabel exp t = case exp of
  EVar x0 s -> EVar (setTypeInLabel x0 t) s
  ELit x0 lit -> ELit (setTypeInLabel x0 t) lit
  EApp x0 exp' exp2 -> EApp (setTypeInLabel x0 t) exp' exp2
  EAbs x0 s exp' -> EAbs (setTypeInLabel x0 t) s exp'
  ELet x0 s exp' exp2 -> ELet (setTypeInLabel x0 t) s exp' exp2
  ELetRec x0 s exp' exp2 -> ELetRec (setTypeInLabel x0 t) s exp' exp2
  EIf x0 exp' exp2 exp3 -> EIf (setTypeInLabel x0 t) exp' exp2 exp3
  EBinOp x0 exp' ob exp2 -> EBinOp (setTypeInLabel x0 t) exp' ob exp2
  EUnOp x0 ou exp' -> EUnOp (setTypeInLabel x0 t) ou exp'
  EMatch x0 s pbs -> EMatch (setTypeInLabel x0 t) s pbs


-- TODO: w tym momencie tłumaczenie zmiennych zmiennych 
-- jakoś tak po porostu stwierdza że każda zmienna to ADT 
-- zapewne dlatego że są parsowane jako Ident

notTypedIdent :: NTIdent  -> Ident
notTypedIdent (UnTypedIndent ma id) = id 

tiname :: TIdent -> String
tiname (TypedIdentifier ma id ta) =  itname id

ntiname :: NTIdent' a -> String
ntiname (UnTypedIndent ma id) =  itname id


translateDef :: A.Def' Pos -> Expected (Exp Label, String)
translateDef (A.DefinitionTyped a (A.Ident i) ids t exp) = do
    exp' <- translate exp
    labels <- mapM makeLabelT ids
    let (ids'', tys) = unzip labels
    let ids' = map tiname ids

    tr <- makeLabelAnn t

    let ty = makeFunType (tys ++ [tr])


    if map itname ids'' /= ids' then
      Left "Incorrect names"
    else  case ids' of
          [] -> return (setTypeLabel exp' ty, i)
          iss -> return (setTypeLabel app ty, i)
            where app = foldr (EAbs (a, Nothing)) exp' iss

translateDef (A.DefinitionUntyped a (A.Ident i) ids exp) = do
    exp' <- translate exp
    labels <- mapM makeEmptyLabel ids
    let (ids'', tys) = unzip labels
    let ids' = map ntiname ids

    -- tr <- makeLabel' t

    -- let ty = makeFunType (tys ++ [tr])


    if map itname ids'' /= ids' then
      Left "Incorrect names"
    else  case ids' of
          [] -> return (setTypeLabel exp' Nothing, i)
          iss -> return (setTypeLabel app Nothing, i)
            where app = foldr (EAbs (a, Nothing)) exp' iss



instance Translatable (A.Program' Pos) where
    translate (A.ProgramText a []) =
        return $ EVar (a, Nothing) "main"
    translate (A.ProgramText a1 (x:xs)) = case x of
      A.TopDef a def -> (do
        (exp', name) <- translateDef def
        inner <- translate (A.ProgramText a1 xs)
        -- TODO - Nothing in the next line should be replaced by type
        return $ ELetRec (a1, Nothing) name exp' inner
        )
      A.TopType a td -> error "Type definitions should not be translated."

instance Translatable A.BoolLiteral where
    translate (A.BoolLiteral b) =
        return $ ELit (Nothing, Nothing) (LBool (Nothing, Nothing) b') where
            b' = b == "True"

instance Translatable (A.Lambda' Pos) where
  translate :: A.Lambda' Pos -> Expected (Exp Label)
  translate (A.AnonymousFunctionTyped a ids t exp) = do
    (exp', _) <- translateDef (A.DefinitionTyped a (A.Ident "__anonymous__") ids t exp)
    return exp'
  translate (A.AnonymousFunctionUntyped a ids exp) = do
    (exp', _) <- translateDef (A.DefinitionUntyped a (A.Ident "__anonymous__") ids exp)
    return exp'

instance Translatable (A.Val' Pos) where
  translate :: A.Val' Pos -> Expected (Exp Label)
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



-- TODO - Nothings in the next lines should be replaced by type
instance Translatable (A.Expr' Pos) where
  translate :: A.Expr' Pos -> Expected (Exp Label)
  translate (A.EVar a (A.Ident i)) = return $ EVar (a, Nothing) i
  translate (A.ELitInt a n) = return $ ELit (a, Nothing) (LInt (a, Nothing) n)
  translate (A.ELitBool a b) = translate b
  translate (A.ELambda a l) = translate l
  translate (A.ELitList a vals) = do
        vals' <- mapM translate vals
        return $ ELit (a, Nothing) (LList (a, Nothing) vals')

  translate (A.EApp a e1 []) = translate e1

  translate (A.EApp a e1 xs) = do
        e1' <- translate e1
            -- x' = translate x
        xs' <- mapM translate xs
        return $ foldl (EApp (a, Nothing)) e1' xs'

  translate (A.Neg a exp) = do
       exp' <- translate exp
       return $ EUnOp (a, Nothing) OpNeg exp'

  translate (A.Not a exp) = do
       exp' <- translate exp
       return $ EUnOp (a, Nothing) OpNot exp'


  translate (A.EMul a exp1 op exp2) = do
      exp1' <- translate exp1
      exp2' <- translate exp2
      let op' = translateOp op
      return $ EBinOp (a, Nothing) exp1' op' exp2'


  translate (A.EAdd a exp1 op exp2) = do
      exp1' <- translate exp1
      exp2' <- translate exp2
      let op' = translateOp op
      return $ EBinOp (a, Nothing) exp1' op' exp2'
  translate (A.ERel a exp1 op exp2) = do
    exp1' <- translate exp1
    exp2' <- translate exp2
    let op' = translateOp op
    return $ EBinOp (a, Nothing)  exp1' op' exp2'
  translate (A.EAnd a exp1 exp2) = do
    exp1' <- translate exp1
    exp2' <- translate exp2
    return $ EBinOp (a, Nothing) exp1' OpAnd exp2'
  translate (A.EOr a exp1 exp2) = do
    exp1' <- translate exp1
    exp2' <- translate exp2
    return $ EBinOp (a, Nothing) exp1' OpOr exp2'

  translate (A.EIf a exp exp1 exp2) = do
    exp' <- translate exp
    exp1' <- translate exp1
    exp2' <- translate exp2
    return $ EIf (a, Nothing) exp' exp1' exp2'
  translate (A.ELet a def exp) = do
    (def', name) <- translateDef def
    exp' <- translate exp
    return $ ELet (a, Nothing) name def' exp'
  translate (A.ELetRec a def exp) = do
      (def', name) <- translateDef def
      exp' <- translate exp
      return $ ELetRec (a, Nothing) name def' exp'

  translate (A.EMatch a (A.Ident i) pbs) = do
    branches <- mapM translateBranch pbs
    return $ EMatch (a, Nothing) i branches
  translate (A.ConsLit a x xs) =
     translate (A.EApp a (A.EVar a (A.Ident "Cons")) [x, xs])


translateBranch :: A.PatternBranch' Pos -> Expected (PatternBranch Label)
translateBranch (BranchPattern a pat ex) = do
    pat' <- translatePattern pat
    ex' <- translate ex
    return $ PatternBranch pat' ex'


translatePatternField :: A.Field' Pos -> Expected (Pattern Label)
translatePatternField field = case field of
  ConstrField a pat -> translatePattern pat
  ConstrFieldIdent a (A.Ident i) -> return $ PatternIdent (a, Nothing) i


isStringLower :: String -> Bool
isStringLower s =
  let c = head s in
    isLower c


translatePattern  :: A.Pattern' Pos -> Expected (Pattern Label)
translatePattern pat = case pat of
  A.PatternLiteral a lit -> Left "Literal Patterns are not supported yet."
  A.PatternConstr a (A.Ident i) fis ->
    if isStringLower i
      then
        (
          if null fis
            then
              Right (PatternIdent (a, Nothing) i)
            else
              Left "lowercase name should be an ident, not constr")
      else (do
        fis' <- mapM translatePatternField fis
        return $ PatternConstr (a, Nothing) i fis'
      )


    -- 
  A.PatternList a lp ->
      case lp of
        A.PatternEmptyList a -> return $ PatternEmptyList (a, Nothing)
        A.PatternConsList a p1 p2 -> (do
          p1' <- translatePattern p1
          p2' <- translatePattern p2
          return $ PatternConsList (a, Nothing) p1' p2'
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



changeLiteral :: (a -> b) -> Lit a -> Lit b
changeLiteral f l = case l of
  LInt a n -> LInt (f a) n
  LBool a b -> LBool (f a) b
  LList a exps -> LList (f a) (map (changeLabel f) exps)

changePattern :: (a -> b) -> Pattern a -> Pattern b
changePattern f pat = case pat of
  PatternEmptyList a -> PatternEmptyList (f a)
  PatternConsList a pat' pat_a -> PatternConsList (f a) (changePattern f pat') (changePattern f pat_a)
  PatternConstr a s pats -> PatternConstr (f a) s (map (changePattern f) pats)
  PatternIdent a s -> PatternIdent (f a) s

changePatternBranch :: (a -> b) -> PatternBranch a -> PatternBranch b
changePatternBranch f (PatternBranch pat exp) =
    PatternBranch (changePattern f pat) (changeLabel f exp)

changeLabel :: (a -> b) ->  Exp a -> Exp b
changeLabel f exp = case exp of
  EVar a s -> EVar (f a) s
  ELit a lit -> ELit (f a) (changeLiteral f lit)
  EApp a exp' exp_a -> EApp (f a) (changeLabel f exp') (changeLabel f exp_a)
  EAbs a s exp' -> EAbs (f a) s (changeLabel f exp')
  ELet a s exp' exp_a -> ELet (f a) s (changeLabel f exp') (changeLabel f exp_a)
  ELetRec a s exp' exp_a -> ELetRec (f a) s (changeLabel f exp') (changeLabel f exp_a)
  EIf a exp' exp_a exp'' -> EIf (f a) (changeLabel f exp') (changeLabel f exp_a) (changeLabel f exp'')
  EBinOp a exp' ob exp_a -> EBinOp (f a) (changeLabel f exp') ob (changeLabel f exp_a)
  EUnOp a ou exp' -> EUnOp (f a) ou (changeLabel f exp')
  EMatch a s pbs -> EMatch (f a) s (map (changePatternBranch f) pbs)