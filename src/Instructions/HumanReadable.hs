module Instructions.HumanReadable (writeHumanReadable) where

import           Types (Instruction(..), Value(..))
import           GHC.IO.Handle (Handle)
import Data.List (intercalate)
import GHC.IO.Handle.Text (hPutStrLn)
import Data.Char (toUpper)

showValue :: Value -> String
showValue Nil = "nil"
showValue (Integer x) = show x
showValue (Float x) = show x
showValue (Char x) = show x
showValue (Boolean x) = show x
showValue (Str x) = show x

humanInstruction :: Instruction -> String
humanInstruction (Function name args) = "Function " ++ name ++ "(" ++ intercalate ", " args ++ ")"
humanInstruction (Push x) = "\tPUSH " ++ showValue x
humanInstruction (PushSymbol x) = "\tPUSH symbol \"" ++ x ++ "\""
humanInstruction x = "\t" ++ map toUpper (show x)

writeHumanReadable :: Handle -> [Instruction] -> IO ()
writeHumanReadable _ [] = return ()
writeHumanReadable fd (x:xs) = hPutStrLn fd (humanInstruction x)
  >> writeHumanReadable fd xs
