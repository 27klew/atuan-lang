-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- | Program to test parser.

{-# LANGUAGE OverloadedStrings #-}


module Main where



import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile, print
  , map, Maybe (..), Bool (..), not
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import Atuan.Abs   (Program, TVar' (TypeVariable), Constr' (DataConstructor), Ident (..), Type' (TypeVar), TypeAnnot' (TypeAnnotation), Top' (..), Program' (..))
import Atuan.Lex   ( Token, mkPosToken )
import Atuan.Par   ( pProgram, myLexer )
import Atuan.Print ( Print, printTree )
import Atuan.Skel  ()
import Atuan.CollectTypes ( collect , ADTs (ADTs, from_name, from_constr), ADT (ADT) )
import Data.List ( (++), map, concat, unlines, intercalate, filter )
import Data.Map (elems, toList, keys, Map, lookup, filter)

import Atuan.Translate (Translatable (translate), translateConstrs)
import qualified Atuan.AlgorithmW  as W (ti, test, testDefault, testEnv, testEnv')
import qualified Atuan.Evaluate as Eval (eval, Val'(..), testEval)


debug = False

putStrLn' :: String -> IO ()
putStrLn' x = when debug (putStrLn x)


type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p


-- showType :: Show a => (Ident, ([TVar' a], [Constr' a])) -> String

-- showType :: Show a => Ident -> Map Ident ([TVar' a], [Ident]) -> Map Ident (Type' a) -> String
-- showType ident types constrs = "aaa"
--     -- "Type: " ++ show ident ++ "\n\tvars: " ++ show vars  ++ "\n\tconstr: " ++ show contrs

showVars :: [TVar' a ] -> String
showVars vars =
  show $ map (\(TypeVariable _ (Ident n)) -> n) vars


showConstructor (DataConstructor _ (Ident n) (TypeAnnotation _ t)) =
    "cname: " ++ n ++
    "\n\ttype: " ++  show t


showConstructors :: Show a => [Ident] -> ADTs a -> String
showConstructors con types =
  intercalate "\n" $
      map ((\(Just (d)) -> showConstructor d) . (\x -> lookup x (from_constr types))) con


showType :: Show a =>  ADTs a -> Ident -> String
showType types name =
    let Just (ADT (Ident name') vars con) = lookup name (from_name types) in
      "{\nname: " ++ name'  ++
      "\nvars: " ++ showVars vars ++
      "\ncons: \n" ++ showConstructors con types ++ "\n}"

    -- show (Data.Map.lookup name (from_name types))


showTypes :: Show a =>  ADTs a -> String
showTypes (ADTs types constr) =
  "ADTs: " ++ intercalate ", " (map show (keys types)) ++ "\n\n"
    ++ intercalate ndashes  (map (showType (ADTs types constr)) (keys types))


    -- ++ ndashes ++ intercalate ndashes (map showType ((keys types) types constr))

  -- intercalate ndashes (map showType (toList x)) 


dashes = "--------------------------------------------------------------------------"
ndashes = "\n" ++ dashes ++ "\n"

ndash s = ndashes ++ dashes ++ "\n\n\t\t\t\t" ++ s ++ "\n" ++ dashes ++ ndashes


isType :: Top' a -> Bool
isType t = case t of
  TopDef a de -> False
  TopType a td -> True


run ::  Verbosity -> ParseFun Program -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn' "\nParse Successful!"
      when debug (showTree v tree)
      putStrLn' "\nSummary!"

      let (ProgramText a tops) = tree

      let types = collect tree
      let defs = ProgramText a $  Data.List.filter(not . isType) tops


      case  types of
        Left str -> putStrLn $ "error: " ++ str
        Right adts -> do
          putStrLn' $ ndash "types"  ++ showTypes adts
          let types = translateConstrs adts
          putStrLn' $ ndashes ++ ndashes ++ ndashes ++ ndashes ++ "types again" ++ ndashes
          when debug (print types)


      -- let Right types' = types
      -- let typed = typecheck types' tree

          putStrLn' $ ndashes ++ ndashes ++ ndashes ++ ndashes

      -- case  typed of
      --   Left str -> putStrLn $ "error: " ++ str
      --   Right typed' -> do
      --     putStrLn "\n\n\n\nTypeCheck Successful!"
      --     showTree v typed'

          let treeExp = translate defs
          case treeExp of
            Left str -> putStrLn $ str ++ "\n "
            Right treeExp -> (do

              putStrLn' $ show treeExp

              putStrLn' $ ndashes ++ ndashes ++ ndashes ++ ndashes
              putStrLn' $ ndashes ++ ndashes ++ ndashes ++ ndashes


              res <- W.testEnv' types treeExp

              case res of
                Left err  ->  putStrLn $ show err ++ "\n " ++ err ++ "\n"
                Right ty   ->  ( do
                  putStrLn $  "\nType of main: " ++ show ty
                  let val = Eval.testEval adts treeExp

                  (
                    case val of
                      Left str -> putStrLn $ "Something went wrong in the calcutation: " ++ str
                      Right val' -> putStrLn $ "value: " ++ show val'
                    )
                  )
              )
      -- let typed = ti  treeExp

          putStrLn' "\n\n\nThat's It!"
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pProgram
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs

