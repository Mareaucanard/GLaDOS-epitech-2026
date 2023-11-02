module Vm.Builtin.IO (ioMap) where

import           Types
import           System.IO (openFile, IOMode(WriteMode, ReadMode, AppendMode, ReadWriteMode), hGetLine, hClose, hIsEOF, stdin, hPutStr, hFlush)
import           System.Exit (die)
import           Vm.VmTypes
import           Vm.Utils
import           Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L
import           System.Time.Extra (sleep)


ioMap :: [(String, Symbol)]
ioMap = [ ("open", BuiltIn open)
        , ("print", BuiltIn printCall)
        , ("close", BuiltIn closeCall)
        , ("input", BuiltIn inputCall)
        , ("write", BuiltIn writeCall)
        , ("read", BuiltIn readCall)
        , ("fetch", BuiltIn fetch)
        , ("throw", BuiltIn throwCall)
        , ("sleep", BuiltIn sleepCall)]

printCall :: BuiltInFunc
printCall s m = case popN s m 1 of
  Right (f, [x]) -> putStrLn (showFlat x) >> none f
  Right (_, _)   -> die "Print takes one argument but none given"
  Left e         -> die e

openLogic :: String -> Char -> Stack -> IO (StackValue, Stack)
openLogic filename 'w' s = openFile filename WriteMode
  >>= \f -> return (Flat (File f), s)
openLogic filename 'r' s = openFile filename ReadMode
  >>= \f -> return (Flat (File f), s)
openLogic filename 'a' s = openFile filename AppendMode
  >>= \f -> return (Flat (File f), s)
openLogic filename 'b' s = openFile filename ReadWriteMode
  >>= \f -> return (Flat (File f), s)
openLogic _ _ _ = die "Open: invalid mode"

open :: BuiltInFunc
open s m = case popN s m 2 of
  Right (f_stack, [V (Str filename), V (Char mode)])
    -> openLogic filename mode f_stack
  Right (_, [V (Str _), _]) -> die "Open: invalid type for filename"
  Right (_, [_, V (Char _)]) -> die "Open: invalid type for mode"
  Right (_, _) -> die "Open: Wrong number of arguments"
  Left e -> die e

writeCall :: BuiltInFunc
writeCall s m = case popN s m 2 of
  Right (f_stack, [File file, V (Str text)])
    -> hPutStr file text >> hFlush file >> none f_stack
  Right (_, [File _, _]) -> die "Write: not a string"
  Right (_, [_, V (Str _)]) -> die "Write: not a file"
  Right (_, _) -> die "Write: wrong number of arguments"
  Left e -> die e

readCall :: BuiltInFunc
readCall s m = case popN s m 1 of
  Right (f_stack, [File f]) -> hIsEOF f
    >>= \empty
    -> if empty
       then none f_stack
       else hGetLine f >>= \line -> return (Flat (V (Str line)), f_stack)
  Right (_, _) -> die "Read: not a file"
  Left e -> die e

closeCall :: BuiltInFunc
closeCall s m = case popN s m 1 of
  Right (f_stack, [File f]) -> hClose f >> none f_stack
  Right (_, _) -> die "Close: not a file"
  Left e -> die e

inputCall :: BuiltInFunc
inputCall s _ = hIsEOF stdin
  >>= \empty -> if empty
                then none s
                else getLine >>= \line -> return (Flat (V (Str line)), s)

throwCall :: BuiltInFunc
throwCall s m = case popN s m 1 of
  Right (_, [x]) -> die $ "Error thrown: " ++ showFlat x
  Right _        -> die "Throw: Wrong number of arguments"
  Left e         -> die e

fetch :: BuiltInFunc
fetch s m = case popN s m 1 of
  Right (f_stack, [V (Str url)]) -> simpleHttp url
    >>= \x -> return (Flat (V (Str (readByteString x))), f_stack)
  Right (_, [x]) -> die $ "Fetch: expected string but got " ++ typeOfVal x
  _ -> die "Fetch: Die"

readByteString :: L.ByteString -> String
readByteString bs = case L.uncons bs of
  Nothing -> []
  Just (x, leftovers) -> x:readByteString leftovers

sleepCall :: BuiltInFunc
sleepCall s m = case popN s m 1 of
  Right (s', [V (Integer t)]) -> sleep (fromIntegral t) >> none s'
  Right (s', [V (Float t)]) -> sleep t >> none s'
  Right (_, [_]) -> die $ name ++ ": invalid type"
  Right _ -> die $ name ++ ": Wrong number of arguments"
  Left e -> die e
  where
    name = "sleep"
