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
  , map
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import Atuan.Abs   (Program, TVar', Constr')
import Atuan.Lex   ( Token, mkPosToken )
import Atuan.Par   ( pProgram, myLexer )
import Atuan.Print ( Print, printTree )
import Atuan.Skel  ()
import Atuan.CollectTop ( collect , TypeEnv )
import Data.List
import Data.Map (elems)




type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p


showType :: Show a => ([TVar' a], [Constr' a]) -> String
showType (vars, contrs) = "vars: " ++ show vars  ++ "constr: " ++ show contrs

showMap :: Show a =>  TypeEnv a -> String
showMap x = intercalate  " \n \n \n " (map showType (elems x)) 


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
      putStrLn "\nParse Successful!"
      showTree v tree
      putStrLn "\nSummary!"
      
      case collect tree of
        Left str -> putStrLn $ "error: " ++ str
        Right map -> putStrLn $ "types: " ++ showMap map



      putStrLn "\n\n\nThat's It!"
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

