-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Atuan.

module Atuan.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Atuan.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Atuan.Abs.Ident where
  prt _ (Atuan.Abs.Ident i) = doc $ showString i
instance Print Atuan.Abs.BoolLiteral where
  prt _ (Atuan.Abs.BoolLiteral i) = doc $ showString i
instance Print (Atuan.Abs.Program' a) where
  prt i = \case
    Atuan.Abs.ProgramText _ tops -> prPrec i 0 (concatD [prt 0 tops])

instance Print (Atuan.Abs.Top' a) where
  prt i = \case
    Atuan.Abs.TopDef _ def -> prPrec i 0 (concatD [prt 0 def])
    Atuan.Abs.TopType _ typedef -> prPrec i 0 (concatD [prt 0 typedef])

instance Print (Atuan.Abs.Def' a) where
  prt i = \case
    Atuan.Abs.DefinitionTyped _ id_ tidents typeannot expr -> prPrec i 0 (concatD [prt 0 id_, prt 0 tidents, prt 0 typeannot, doc (showString "="), prt 0 expr])
    Atuan.Abs.DefinitionUntyped _ id_ ntidents expr -> prPrec i 0 (concatD [prt 0 id_, prt 0 ntidents, doc (showString "="), prt 0 expr])

instance Print (Atuan.Abs.NTIdent' a) where
  prt i = \case
    Atuan.Abs.UnTypedIndent _ id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print (Atuan.Abs.TIdent' a) where
  prt i = \case
    Atuan.Abs.TypedIdentifier _ id_ typeannot -> prPrec i 0 (concatD [doc (showString "("), prt 0 id_, prt 0 typeannot, doc (showString ")")])

instance Print (Atuan.Abs.OTIdent' a) where
  prt i = \case
    Atuan.Abs.OptionallyTypedIdentifier _ tident -> prPrec i 0 (concatD [prt 0 tident])
    Atuan.Abs.SkippedTypeIdentifier _ id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print (Atuan.Abs.TypeDef' a) where
  prt i = \case
    Atuan.Abs.TypeDefinition _ id_ tvars constrs -> prPrec i 0 (concatD [doc (showString "data"), prt 0 id_, prt 0 tvars, doc (showString "where"), prt 0 constrs])

instance Print (Atuan.Abs.TVar' a) where
  prt i = \case
    Atuan.Abs.TypeVariable _ id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print [Atuan.Abs.TVar' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print (Atuan.Abs.Constr' a) where
  prt i = \case
    Atuan.Abs.DataConstructor _ id_ typeannot -> prPrec i 0 (concatD [prt 0 id_, prt 0 typeannot])

instance Print [Atuan.Abs.Constr' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Atuan.Abs.TypeAnnot' a) where
  prt i = \case
    Atuan.Abs.TypeAnnotation _ type_ -> prPrec i 0 (concatD [doc (showString "::"), prt 0 type_])

instance Print (Atuan.Abs.OptTypeAnnot' a) where
  prt i = \case
    Atuan.Abs.OptionalTypeAnnotation _ typeannot -> prPrec i 0 (concatD [prt 0 typeannot])
    Atuan.Abs.SkippedTypeAnnotation _ -> prPrec i 0 (concatD [])

instance Print [Atuan.Abs.Top' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x, doc (showString ";")]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [Atuan.Abs.Ident] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Atuan.Abs.OTIdent' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print [Atuan.Abs.TIdent' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print [Atuan.Abs.NTIdent' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print (Atuan.Abs.Block' a) where
  prt i = \case
    Atuan.Abs.CurlyBlock _ expr -> prPrec i 0 (concatD [doc (showString "{"), prt 0 expr, doc (showString "}")])

instance Print (Atuan.Abs.Lambda' a) where
  prt i = \case
    Atuan.Abs.AnonymousFunctionTyped _ tidents typeannot expr -> prPrec i 0 (concatD [doc (showString "(lambda"), prt 0 tidents, prt 0 typeannot, doc (showString "=>"), prt 0 expr, doc (showString ")")])
    Atuan.Abs.AnonymousFunctionUntyped _ ntidents expr -> prPrec i 0 (concatD [doc (showString "(lambda"), prt 0 ntidents, doc (showString "=>"), prt 0 expr, doc (showString ")")])

instance Print (Atuan.Abs.Expr' a) where
  prt i = \case
    Atuan.Abs.EVar _ id_ -> prPrec i 9 (concatD [prt 0 id_])
    Atuan.Abs.ELitInt _ n -> prPrec i 9 (concatD [prt 0 n])
    Atuan.Abs.ELitBool _ boolliteral -> prPrec i 9 (concatD [prt 0 boolliteral])
    Atuan.Abs.ELambda _ lambda -> prPrec i 8 (concatD [prt 0 lambda])
    Atuan.Abs.ELitList _ vals -> prPrec i 9 (concatD [doc (showString "["), prt 0 vals, doc (showString "]")])
    Atuan.Abs.EApp _ expr exprs -> prPrec i 7 (concatD [prt 9 expr, prt 9 exprs])
    Atuan.Abs.Neg _ expr -> prPrec i 6 (concatD [doc (showString "-"), prt 7 expr])
    Atuan.Abs.Not _ expr -> prPrec i 6 (concatD [doc (showString "!"), prt 7 expr])
    Atuan.Abs.EMul _ expr1 mulop expr2 -> prPrec i 5 (concatD [prt 5 expr1, prt 0 mulop, prt 6 expr2])
    Atuan.Abs.EAdd _ expr1 addop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 addop, prt 5 expr2])
    Atuan.Abs.ERel _ expr1 relop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 relop, prt 4 expr2])
    Atuan.Abs.EAnd _ expr1 expr2 -> prPrec i 2 (concatD [prt 3 expr1, doc (showString "&&"), prt 2 expr2])
    Atuan.Abs.EOr _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "||"), prt 1 expr2])
    Atuan.Abs.EMatch _ id_ patternbranchs -> prPrec i 1 (concatD [doc (showString "match"), prt 0 id_, doc (showString "with"), prt 0 patternbranchs])
    Atuan.Abs.EIf _ expr1 expr2 expr3 -> prPrec i 1 (concatD [doc (showString "if"), prt 2 expr1, doc (showString "then"), prt 2 expr2, doc (showString "else"), prt 2 expr3])
    Atuan.Abs.ELet _ def expr -> prPrec i 1 (concatD [doc (showString "let"), prt 0 def, doc (showString "in"), prt 2 expr])
    Atuan.Abs.ELetRec _ def expr -> prPrec i 1 (concatD [doc (showString "letrec"), prt 0 def, doc (showString "in"), prt 2 expr])
    Atuan.Abs.ConsLit _ expr1 expr2 -> prPrec i 8 (concatD [prt 9 expr1, doc (showString ":"), prt 9 expr2])

instance Print (Atuan.Abs.Val' a) where
  prt i = \case
    Atuan.Abs.ValList _ expr -> prPrec i 0 (concatD [prt 9 expr])

instance Print [Atuan.Abs.Val' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Atuan.Abs.PatternBranch' a) where
  prt i = \case
    Atuan.Abs.BranchPattern _ pattern_ expr -> prPrec i 0 (concatD [doc (showString "("), prt 0 pattern_, doc (showString ")"), doc (showString ">>>"), prt 2 expr])

instance Print [Atuan.Abs.PatternBranch' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Atuan.Abs.Expr' a] where
  prt 9 [x] = concatD [prt 9 x, doc (showString " ")]
  prt 9 (x:xs) = concatD [prt 9 x, doc (showString " "), prt 9 xs]
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print (Atuan.Abs.AddOp' a) where
  prt i = \case
    Atuan.Abs.Plus _ -> prPrec i 0 (concatD [doc (showString "+")])
    Atuan.Abs.Minus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (Atuan.Abs.MulOp' a) where
  prt i = \case
    Atuan.Abs.Times _ -> prPrec i 0 (concatD [doc (showString "*")])
    Atuan.Abs.Div _ -> prPrec i 0 (concatD [doc (showString "/")])
    Atuan.Abs.Mod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (Atuan.Abs.RelOp' a) where
  prt i = \case
    Atuan.Abs.LTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    Atuan.Abs.LE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    Atuan.Abs.GTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    Atuan.Abs.GE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    Atuan.Abs.EQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    Atuan.Abs.NE _ -> prPrec i 0 (concatD [doc (showString "!=")])

instance Print (Atuan.Abs.ListPattern' a) where
  prt i = \case
    Atuan.Abs.PatternEmptyList _ -> prPrec i 0 (concatD [doc (showString "[]")])
    Atuan.Abs.PatternConsList _ pattern_1 pattern_2 -> prPrec i 0 (concatD [prt 4 pattern_1, doc (showString ":"), prt 4 pattern_2])

instance Print (Atuan.Abs.Pattern' a) where
  prt i = \case
    Atuan.Abs.PatternLiteral _ literal -> prPrec i 5 (concatD [prt 0 literal])
    Atuan.Abs.PatternConstr _ id_ fields -> prPrec i 4 (concatD [prt 0 id_, prt 0 fields])
    Atuan.Abs.PatternList _ listpattern -> prPrec i 3 (concatD [prt 0 listpattern])

instance Print (Atuan.Abs.Field' a) where
  prt i = \case
    Atuan.Abs.ConstrField _ pattern_ -> prPrec i 0 (concatD [prt 5 pattern_])
    Atuan.Abs.ConstrFieldIdent _ id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print [Atuan.Abs.Field' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print (Atuan.Abs.Literal' a) where
  prt i = \case
    Atuan.Abs.IntLit _ n -> prPrec i 0 (concatD [prt 0 n])
    Atuan.Abs.BoolLit _ boolliteral -> prPrec i 0 (concatD [prt 0 boolliteral])
    Atuan.Abs.LiteralList _ literals -> prPrec i 0 (concatD [doc (showString "["), prt 0 literals, doc (showString "]")])

instance Print [Atuan.Abs.Literal' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Atuan.Abs.Type' a) where
  prt i = \case
    Atuan.Abs.TypeInt _ -> prPrec i 4 (concatD [doc (showString "Int")])
    Atuan.Abs.TypeBool _ -> prPrec i 4 (concatD [doc (showString "Bool")])
    Atuan.Abs.TypeList _ type_ -> prPrec i 4 (concatD [doc (showString "["), prt 0 type_, doc (showString "]")])
    Atuan.Abs.TypeIdent _ id_ -> prPrec i 4 (concatD [prt 0 id_])
    Atuan.Abs.TypeApp _ id_ types -> prPrec i 3 (concatD [doc (showString "("), prt 0 id_, prt 4 types, doc (showString ")")])
    Atuan.Abs.TypeVar _ id_ -> prPrec i 1 (concatD [prt 0 id_])
    Atuan.Abs.TypeFunc _ type_1 type_2 -> prPrec i 1 (concatD [prt 2 type_1, doc (showString "->"), prt 1 type_2])

instance Print [Atuan.Abs.Type' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 4 x, doc (showString " ")]
  prt _ (x:xs) = concatD [prt 4 x, doc (showString " "), prt 4 xs]
