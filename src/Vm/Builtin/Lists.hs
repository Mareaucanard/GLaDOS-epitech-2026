module Vm.Builtin.Lists (listMap) where

import           Types
import           System.Exit (die)
import           Vm.VmTypes
import           Vm.Utils
import           Data.List (intercalate)
import           Vm.Instructions (call)
import           Basement.Compat.Base (Int64)
import           Data.List.Split (splitOn)

listMap :: [(String, Symbol)]
listMap =
  [ ("concat", BuiltIn concatCall)
  , ("head", BuiltIn headCall)
  , ("tail", BuiltIn tailCall)
  , ("reverse", BuiltIn reverseCall)
  , ("join", BuiltIn joinCall)
  , ("any", BuiltIn anyCall)
  , ("all", BuiltIn allCall)
  , ("map", BuiltIn mapCall)
  , ("len", BuiltIn lenCall)
  , ("prepend", BuiltIn prepend)
  , ("append", BuiltIn append)
  , ("nils", BuiltIn nils)
  , ("range", BuiltIn range)
  , ("split", BuiltIn split)]

maybeConcatLists :: [FlatStack] -> Maybe FlatStack
maybeConcatLists ((Tab x):xs) = case maybeConcatLists xs of
  Just (Tab x') -> Just (Tab (x ++ x'))
  _ -> Nothing
maybeConcatLists [] = Just (Tab [])
maybeConcatLists _ = Nothing

maybeConcatString :: [FlatStack] -> Maybe FlatStack
maybeConcatString ((V (Str x)):xs) = case maybeConcatString xs of
  Just (V (Str x')) -> Just $ V (Str (x ++ x'))
  _ -> Nothing
maybeConcatString [] = Just (V (Str []))
maybeConcatString _ = Nothing

concatCall :: BuiltInFunc
concatCall s m = case popN s m 1 of
  Right (f_stack, [Tab ((Tab v):xs)]) -> f maybeConcatLists (Tab v:xs) f_stack
  Right (f_stack, [Tab (V (Str v):xs)])
    -> f maybeConcatString (V (Str v):xs) f_stack
  Right (f_stack, [Tab []]) -> return (Flat (Tab []), f_stack)
  Right (_, [x]) -> die $ "Fetch: expected string but got " ++ typeOfVal x
  _ -> die "Die"
  where
    f = concatCallBack

concatCallBack :: (t -> Maybe FlatStack) -> t -> b -> IO (StackValue, b)
concatCallBack f' l s' = case f' l of
  Nothing -> die "Can't concat non harmonious types"
  Just x  -> return (Flat x, s')

headCall :: BuiltInFunc
headCall s m = case popN s m 1 of
  Right (f_stack, [Tab (x:_)]) -> return (Flat x, f_stack)
  Right (f_stack, [V (Str (x:_))]) -> return (Flat (V (Char x)), f_stack)
  Right (_, [Tab []]) -> die "Head: empty list"
  Right (_, [V (Str [])]) -> die "Head: empty list"
  Right (_, [_]) -> die "Head: invalid type"
  Right _ -> die "Head: Wrong number of arguments"
  Left e -> die e

tailCall :: BuiltInFunc
tailCall s m = case popN s m 1 of
  Right (f_stack, [Tab (_:xs)]) -> return (Flat (Tab xs), f_stack)
  Right (f_stack, [V (Str (_:xs))]) -> return (Flat (V (Str xs)), f_stack)
  Right (_, [Tab []]) -> die "Tail: empty list"
  Right (_, [V (Str [])]) -> die "Tail: empty list"
  Right (_, [_]) -> die "Tail: invalid type"
  Right _ -> die "Tail: Wrong number of arguments"
  Left e -> die e

reverseCall :: BuiltInFunc
reverseCall s m = case popN s m 1 of
  Right (f_stack, [Tab t]) -> return (Flat (Tab (reverse t)), f_stack)
  Right (f_stack, [V (Str t)]) -> return (Flat (V (Str (reverse t))), f_stack)
  Right (_, [_]) -> die "reverse: invalid type"
  Right _ -> die "reverse: Wrong number of arguments"
  Left e -> die e

extractStrings :: [FlatStack] -> Maybe [String]
extractStrings [] = Just []
extractStrings (V (Str x):xs) = case extractStrings xs of
  Nothing  -> Nothing
  Just xs' -> Just $ x:xs'
extractStrings _ = Nothing

joinCall :: BuiltInFunc
joinCall s m = case popN s m 2 of
  Right (f_stack, [V (Str join), Tab xs]) -> case extractStrings xs of
    Nothing -> die "join: can only join strings"
    Just strings -> return (Flat (V (Str l')), f_stack)
      where
        l' = intercalate join strings
  Right (_, [_, _]) -> die "join: invalid type"
  Right _ -> die "join: Wrong number of arguments"
  Left e -> die e

anyCall :: BuiltInFunc
anyCall s m = case popN s m 1 of
  Right (f_stack, [Tab t]) -> return
    (Flat (V (Boolean (V (Boolean True) `elem` t))), f_stack)
  Right (_, [_]) -> die "any: invalid type"
  Right _ -> die "any: Wrong number of arguments"
  Left e -> die e

allCall :: BuiltInFunc
allCall s m = case popN s m 1 of
  Right (f_stack, [Tab t]) -> return
    (Flat (V (Boolean (all (V (Boolean True) ==) t))), f_stack)
  Right (_, [_]) -> die "all: invalid type"
  Right _ -> die "all: Wrong number of arguments"
  Left e -> die e

flattenOrDie :: VarMap -> IO (StackValue, Stack) -> IO FlatStack
flattenOrDie m x = x
  >>= \(v, _) -> case flatten v m of
    Left e   -> die e
    Right x' -> return x'

mapCall :: BuiltInFunc
mapCall s m = case popN s m 1 of
  Right
    (f:s', [Tab t]) -> mapM (flattenOrDie m . (\x -> call (f:Flat x:s') m)) t
    >>= \x -> return (Flat (Tab x), s')
  Right (_, [_]) -> die "map: invalid type"
  Right _ -> die "map: Wrong number of arguments"
  Left e -> die e

lenCall :: BuiltInFunc
lenCall s m = case popN s m 1 of
  Right (f_stack, [Tab t]) -> return (f t, f_stack)
  Right (f_stack, [V (Str t)]) -> return (f t, f_stack)
  Right (_, [_]) -> die "len: invalid type"
  Right _ -> die "len: Wrong number of arguments"
  Left e -> die e
  where
    f l = Flat (V (Integer (fi (length l))))

prepend :: BuiltInFunc
prepend s m = case popN s m 2 of
  Right (f_stack, [Tab t, x]) -> return (Flat (Tab (x:t)), f_stack)
  Right (f_stack, [V (Str t), V (Char c)]) -> return
    (Flat (V (Str (c:t))), f_stack)
  Right (_, [V (Str _), x])
    -> die $ "Can only prepend char to string, not " ++ typeOfVal x
  Right
    (_, [x]) -> die $ "Can only prepend to list or string, not " ++ typeOfVal x
  Right (_, _) -> die "Wrong number of arguments for prepend"
  Left e -> die e

append :: BuiltInFunc
append s m = case popN s m 2 of
  Right (f_stack, [Tab t, x]) -> return (Flat (Tab (t ++ [x])), f_stack)
  Right (f_stack, [V (Str t), V (Char c)]) -> return
    (Flat (V (Str (t ++ [c]))), f_stack)
  Right (_, [V (Str _), x])
    -> die $ "Can only append char to string, not " ++ typeOfVal x
  Right
    (_, [x]) -> die $ "Can only append to list or string, not " ++ typeOfVal x
  Right (_, _) -> die "append: Wrong number of arguments"
  Left e -> die e

listOfNils :: Int64 -> [FlatStack]
listOfNils 0 = []
listOfNils n = V Nil:listOfNils (n - 1)

nils :: BuiltInFunc
nils s m = case popN s m 1 of
  Right (s', [V (Integer size)])
    -> if size < 0
       then die "nils: negative number"
       else return (Flat (Tab (listOfNils size)), s')
  Right (_, [_]) -> die "nils: Invalid type"
  Right (_, _) -> die "nils: Wrong number of arguments"
  Left e -> die e

rangeLogic :: Int64 -> [FlatStack]
rangeLogic n = map f [0 .. (n - 1)]
  where
    f x = V (Integer x)

range :: BuiltInFunc
range s m = case popN s m 1 of
  Right (s', [V (Integer size)])
    -> if size < 0
       then die "range: negative number"
       else return (Flat (Tab (rangeLogic size)), s')
  Right (_, [_]) -> die "range: Invalid type"
  Right (_, _) -> die "range: Wrong number of arguments"
  Left e -> die e

split :: BuiltInFunc
split s m = case popN s m 2 of
  Right (s', [V (Str str), V (Str delim)])
    -> splitCallBack (map toString (splitOn delim str)) s'
  Right (s', [V (Str str), V (Char delim)])
    -> splitCallBack (map toString (splitOn [delim] str)) s'
  Right (_, [_, _]) -> die "split: Invalid type"
  Right (_, _) -> die "split: Wrong number of arguments"
  Left e -> die e

splitCallBack :: Monad m => [FlatStack] -> b -> m (StackValue, b)
splitCallBack x s' = return (Flat (Tab x), s')

toString :: String -> FlatStack
toString x = V (Str x)
