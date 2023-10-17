{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Parsing.Parser
    ( parseChar
    , parseOr
    , parseAnyChar
    , parseUInt
    , parseInt
    , parseDigitStr
    , parseString
    , parseSomeChar
    , parseMaybe
    , Parser(..)
    , first
    , second
    , fuse) where

import           Control.Applicative (Alternative(some), empty, (<|>))
import Data.Int (Int64)
import Data.List (stripPrefix)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap ftc (Parser p1) = Parser
    $ \str -> case p1 str of
      Just (x, str2) -> Just (ftc x, str2)
      Nothing -> Nothing

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \str -> Just (a, str)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> (Parser p1) = Parser $
    \str1 -> case  pf str1 of
       Nothing      -> Nothing
       Just (f, str2) -> case p1 str2 of
        Nothing -> Nothing
        Just (a, str3) -> Just (f a, str3)

instance Alternative Parser where
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = parseOr

  empty :: Parser a
  empty = Parser $ const Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr (Parser p1) (Parser p2) = Parser
  $ \str -> case p1 str of
    Just x   -> Just x
    Nothing -> p2 str

parseSomeChar :: String -> Parser Char
parseSomeChar = foldr ((<|>) . parseChar) empty

parseAnyChar :: Parser Char
parseAnyChar = Parser p
  where p [] = Nothing
        p (x:xs) = Just (x, xs)

parseChar :: Char -> Parser Char
parseChar c = Parser
  $ \case
    (x:xs) | x == c -> Just (c, xs)
    _ -> Nothing

parseString :: String -> Parser String
parseString strToMatch = Parser
  $ \str -> case stripPrefix strToMatch str of
    Just newStr -> Just (strToMatch, newStr)
    Nothing -> Nothing

parseDigitStr :: Parser String
parseDigitStr = some (parseSomeChar ['0' .. '9'])

parseUInt :: Parser Int64
parseUInt = fmap read parseDigitStr

parseInt :: Parser Int64
parseInt =
  fmap read (parseDigitStr <|> fuse <$> parseChar '-' <*> parseDigitStr)

first :: a -> b -> a
first a _ = a

second :: a -> b -> b
second _ b = b

fuse :: a -> [a] -> [a]
fuse x xs = x:xs

parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe (Parser p1) = Parser
  $ \str -> case p1 str of
    Just (res, token) -> Just (Just res, token)
    Nothing           -> Just (Nothing, str)
