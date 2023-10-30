module Instructions.ReadByteCode (readByteCode) where

import           Types (Instruction(..), Value(..))
import qualified Types as T
import qualified Data.ByteString as BS
import           Data.ByteString.Unsafe (unsafeIndex)
import           Data.ByteString (ByteString)
import           Data.Word (Word8, Word64)
import           Basement.Floating (wordToDouble)
import           Data.IntCast
import           Data.Bits

getN :: ByteString -> Int -> Maybe (ByteString, ByteString)
getN l 0 = Just (l, BS.empty)
getN l n = case BS.uncons l of
  Nothing      -> Nothing
  Just (x, xs) -> case getN xs (n - 1) of
    Just (leftovers, data_tail) -> Just (leftovers, BS.cons x data_tail)
    Nothing -> Nothing

toWord64Le :: ByteString -> Word64
toWord64Le s = (fromIntegral (s `unsafeIndex` 7) `unsafeShiftL` 56)
  .|. (fromIntegral (s `unsafeIndex` 6) `unsafeShiftL` 48)
  .|. (fromIntegral (s `unsafeIndex` 5) `unsafeShiftL` 40)
  .|. (fromIntegral (s `unsafeIndex` 4) `unsafeShiftL` 32)
  .|. (fromIntegral (s `unsafeIndex` 3) `unsafeShiftL` 24)
  .|. (fromIntegral (s `unsafeIndex` 2) `unsafeShiftL` 16)
  .|. (fromIntegral (s `unsafeIndex` 1) `unsafeShiftL` 8)
  .|. fromIntegral (s `unsafeIndex` 0)

getWord64Le :: ByteString -> Maybe (ByteString, Word64)
getWord64Le bs = case getN bs 8 of
  Nothing -> Nothing
  Just (leftovers, word) -> Just (leftovers, toWord64Le word)

toChar :: Word8 -> Char
toChar = toEnum . fromEnum

applyToWord64 :: (Word64 -> a) -> ByteString -> Maybe (a, ByteString)
applyToWord64 f bs = case getWord64Le bs of
  Nothing -> Nothing
  Just (leftovers, word) -> Just (f word, leftovers)

getDouble :: ByteString -> Maybe (Value, ByteString)
getDouble = applyToWord64 (Float . wordToDouble)

getInt :: ByteString -> Maybe (Value, ByteString)
getInt = applyToWord64 (Integer . intCastIso)

getCharBS :: ByteString -> Maybe (Value, ByteString)
getCharBS bs = case BS.uncons bs of
  Nothing      -> Nothing
  Just (x, xs) -> Just (Char (toChar x), xs)

getString :: ByteString -> Maybe (Value, ByteString)
getString bs = case readNullString bs of
  Just (str, xs) -> Just (Str str, xs)
  Nothing        -> Nothing

getBool :: ByteString -> Maybe (Value, ByteString)
getBool bs = case BS.uncons bs of
  Just (0, xs) -> Just (Boolean False, xs)
  Just (1, xs) -> Just (Boolean True, xs)
  _ -> Nothing

readNullString :: ByteString -> Maybe (String, ByteString)
readNullString bs = case BS.uncons bs of
  Nothing -> Nothing
  Just (0, leftovers) -> Just ([], leftovers)
  Just (x, leftovers) -> case readNullString leftovers of
    Nothing -> Nothing
    Just (xs, leftovers_final) -> Just (toChar x:xs, leftovers_final)

getArgs :: ByteString -> Maybe ([String], ByteString)
getArgs bs = case BS.uncons bs of
  Nothing -> Nothing
  Just (0, leftover) -> Just ([], leftover)
  Just _ -> case readNullString bs of
    Nothing -> Nothing
    Just (arg_name, leftover) -> case getArgs leftover of
      Nothing -> Nothing
      Just (arg_tail, xs) -> Just (arg_name:arg_tail, xs)

handleFunction :: ByteString -> Maybe (Instruction, ByteString)
handleFunction bs = case readNullString bs of
  Nothing -> Nothing
  Just (name, leftover) -> case getArgs leftover of
    Nothing         -> Nothing
    Just (args, xs) -> Just (Function name args, xs)

readValue :: ByteString -> Maybe (Value, ByteString)
readValue bs = case BS.uncons bs of
  Just (1, xs) -> getInt xs
  Just (2, xs) -> getDouble xs
  Just (3, xs) -> getCharBS xs
  Just (4, xs) -> getString xs
  Just (5, xs) -> getBool xs
  Just (6, xs) -> Just (Nil, xs)
  _ -> Nothing

handlePush :: ByteString -> Maybe (Instruction, ByteString)
handlePush bs = case readValue bs of
  Nothing        -> Nothing
  Just (val, xs) -> Just (Push val, xs)

handlePushSymbol :: ByteString -> Maybe (Instruction, ByteString)
handlePushSymbol bs = case readNullString bs of
  Nothing        -> Nothing
  Just (val, xs) -> Just (PushSymbol val, xs)

handleJIF :: ByteString -> Maybe (Instruction, ByteString)
handleJIF bs = case applyToWord64 intCastIso bs of
  Nothing        -> Nothing
  Just (val, xs) -> Just (JIF val, xs)

handleJump :: ByteString -> Maybe (Instruction, ByteString)
handleJump bs = case applyToWord64 intCastIso bs of
  Nothing        -> Nothing
  Just (val, xs) -> Just (Jump val, xs)

handleList :: ByteString -> Maybe (Instruction, ByteString)
handleList bs = case applyToWord64 intCastIso bs of
  Nothing        -> Nothing
  Just (val, xs) -> Just (LIST val, xs)

handleIndex :: ByteString -> Maybe (Instruction, ByteString)
handleIndex bs = case readNullString bs of
  Nothing        -> Nothing
  Just (val, xs) -> Just (INDEX val, xs)

analyzeByte :: Word8 -> ByteString -> Maybe (Instruction, ByteString)
analyzeByte 0 bs = handleFunction bs
analyzeByte 1 bs = handlePush bs
analyzeByte 2 bs = handlePushSymbol bs
analyzeByte 3 bs = handleJIF bs
analyzeByte 4 bs = handleJump bs
analyzeByte 5 bs = Just (Call, bs)
analyzeByte 6 bs = Just (Set, bs)
analyzeByte 7 bs = Just (ADD, bs)
analyzeByte 8 bs = Just (SUB, bs)
analyzeByte 9 bs = Just (MUL, bs)
analyzeByte 10 bs = Just (DIV, bs)
analyzeByte 11 bs = Just (MOD, bs)
analyzeByte 12 bs = Just (T.EQ, bs)
analyzeByte 13 bs = Just (NEQ, bs)
analyzeByte 14 bs = Just (T.LT, bs)
analyzeByte 15 bs = Just (LET, bs)
analyzeByte 16 bs = Just (T.GT, bs)
analyzeByte 17 bs = Just (GET, bs)
analyzeByte 18 bs = Just (AND, bs)
analyzeByte 19 bs = Just (OR, bs)
analyzeByte 20 bs = Just (NOT, bs)
analyzeByte 21 bs = handleList bs
analyzeByte 22 bs = Just (RET, bs)
analyzeByte 24 bs = handleIndex bs
analyzeByte 25 bs = Just (NEGATIVE, bs)
analyzeByte _ _ = Nothing

readByteLogic :: ByteString -> Maybe [Instruction]
readByteLogic bytes = case BS.uncons bytes of
  Nothing -> Just []
  Just (byte, leftovers1) -> case analyzeByte byte leftovers1 of
    Nothing -> Nothing
    Just (x, leftovers2) -> case readByteLogic leftovers2 of
      Nothing -> Nothing
      Just xs -> Just (x:xs)

parseNString :: ByteString -> Int -> Maybe (String, ByteString)
parseNString bs 0 = Just ([], bs)
parseNString bs n = case BS.uncons bs of
  Nothing -> Nothing
  Just (x, leftovers1) -> case parseNString leftovers1 (n - 1) of
    Nothing -> Nothing
    Just (xs, leftovers2) -> Just (toChar x:xs, leftovers2)

readByteCode :: ByteString -> Either [Instruction] String
readByteCode bytes = case parseNString bytes 4 of
  Just ("GLDB", bytes_2) -> case readByteLogic bytes_2 of
    Nothing -> Right "Weird parsing error in VM file !!"
    Just x  -> Left x
  _ -> Right "Invalid vm file (wrong magic number)"
