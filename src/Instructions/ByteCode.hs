module Instructions.ByteCode (writeByteCode, byteCodeString) where

import           GHC.IO.Handle (Handle)
import           Types (Instruction(..), Value(..))
import qualified Types as T
import           Data.ByteString (hPut, ByteString, hPutStr, pack, singleton)
import qualified Data.ByteString as BS (concat)
import           Data.Word (Word8, Word64)
import           Data.Int (Int64)
import           GHC.Float (castDoubleToWord64)
import           Data.Bits (Bits(shift), (.&.))

codeOf :: Instruction -> Int
codeOf (Function _ _) = 0
codeOf (Push _) = 1
codeOf (PushSymbol _) = 2
codeOf (JIF _) = 3
codeOf (Jump _) = 4
codeOf Call = 5
codeOf Set = 6
codeOf ADD = 7
codeOf SUB = 8
codeOf MUL = 9
codeOf DIV = 10
codeOf MOD = 11
codeOf T.EQ = 12
codeOf NEQ = 13
codeOf T.LT = 14
codeOf LET = 15
codeOf T.GT = 16
codeOf T.GET = 17
codeOf AND = 18
codeOf OR = 19
codeOf NOT = 20
codeOf (LIST _) = 21
codeOf RET = 22
codeOf TERNARY = 23
codeOf (INDEX _) = 24
codeOf NEGATIVE = 25

stringToByteString :: String -> ByteString
stringToByteString v = pack (map charToWord8 v)

nullString :: String -> ByteString
nullString v = pack (map charToWord8 (v ++ "\0"))

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

int64ToWord64 :: Int64 -> Word64
int64ToWord64 = fromIntegral

doubleToWord64 :: Double -> Word64
doubleToWord64 = castDoubleToWord64

getByte :: (Bits a, Num a) => Int -> a -> a
getByte bs n = shift (relevantBits bs n) (shiftNum bs)
  where
    bitMask b = sum $ map (2 ^) [8 * b - 8 .. 8 * b - 1]

    relevantBits b num = num .&. bitMask b

    shiftNum b = 8 - 8 * b

encodeWord64 :: Word64 -> [Word8]
encodeWord64 bs = [fromIntegral (getByte i bs) | i <- [1 .. 8]]

showInt64 :: Int64 -> ByteString
showInt64 x = pack (encodeWord64 (int64ToWord64 x))

showDouble :: Double -> ByteString
showDouble x = pack (encodeWord64 (doubleToWord64 x))

valueToBytes :: Value -> ByteString
valueToBytes (Integer x) = BS.concat [singleton 1, showInt64 x]
valueToBytes (Float f) = BS.concat [singleton 2, showDouble f]
valueToBytes (Char c) = BS.concat [singleton 3, singleton $ charToWord8 c]
valueToBytes (Str str) = BS.concat [singleton 4, nullString str]
valueToBytes (Boolean True) = BS.concat [singleton 5, singleton 1]
valueToBytes (Boolean False) = BS.concat [singleton 5, singleton 0]
valueToBytes Nil = singleton 6

instToByteTail :: Instruction -> [ByteString]
instToByteTail (Function name args) =
  [nullString name, BS.concat (map nullString args), singleton 0]
instToByteTail (Push x) = [valueToBytes x]
instToByteTail (PushSymbol s) = [nullString s]
instToByteTail (JIF n) = [showInt64 n]
instToByteTail (Jump n) = [showInt64 n]
instToByteTail (LIST n) = [showInt64 n]
instToByteTail (INDEX sym) = [nullString sym]
instToByteTail _ = []

instToByteString :: Instruction -> ByteString
instToByteString x = BS.concat
  (singleton (intToWord8 (codeOf x)):instToByteTail x)

byteCodeString :: [Instruction] -> ByteString
byteCodeString l = BS.concat
  [stringToByteString "GLDB", BS.concat (map instToByteString l)]

byteCodeList :: Handle -> [Instruction] -> IO ()
byteCodeList _ [] = return ()
byteCodeList file (x:xs) = hPut file (instToByteString x)
  >> byteCodeList file xs

writeByteCode :: Handle -> [Instruction] -> IO ()
writeByteCode file insts = hPutStr file magic_code >> byteCodeList file insts
  where
    magic_code = stringToByteString "GLDB"
