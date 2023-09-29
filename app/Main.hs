module Main (main) where

import Ast
import Data.Char (toLower)
import Lib (parseString)
import Interpreter (parseLine)
import DefaultSymbol (defaultSymbols)
import System.Random
import System.IO
import Types (Ast(..))
import System.Environment   
import qualified Data.Map.Lazy as Map

processInput :: IO String
processInput = do
  line <- parseLine 0
  return $ map toLower line

handleLine :: String -> VarMap -> IO ()
handleLine line m = case parseString line of
  Right x -> putStrLn x
  Left sexpr -> case sexprToAST sexpr of
    Right msg -> putStrLn msg
    Left ast -> case evalAst ast m of
      Right err -> putStrLn err
      Left (result, newVars) -> print result >> loop newVars --Changer print result pour clean l'output

-- This is our main loop, it handles when to exit
loop :: VarMap -> IO ()
loop m = do
  putStr "Glados> "
  hFlush stdout
  line <- processInput
  case line of
    "quit" -> return ()
    _ -> handleLine line m

welcomMessage :: [String] -> IO ()
welcomMessage args = case args of
  [] -> putStr "Welcome to GLaDOS, the lisp interpreter\n"
  _ -> putStrLn $ "Welcome to GLaDOS"

main :: IO ()
main = do
  args <- getArgs
  welcomMessage args
  seed <- randomIO :: IO Int
  loop (Map.insert "seed" (Value seed) defaultSymbols)
