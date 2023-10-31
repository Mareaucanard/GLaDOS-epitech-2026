module Parsing.Tokenize (tokenize) where

import           Types
import           Parsing.Parser
import           Control.Applicative ((<|>), Alternative(some))
import           GHC.Base (many, Alternative(empty))

between :: Parser a -> Parser b -> Parser c -> Parser c
between a b c = getMiddle <$> a <*> c <*> b
  where
    getMiddle _ x _ = x

parseInteger :: Parser Token
parseInteger = applyInteger <$> parseInt
  where
    applyInteger x = Constant (Integer x)

parseDoubleString :: Parser String
parseDoubleString = join <$> parseMaybe (parseChar '-')
  <*> parseDigitStr
  <*> parseChar '.'
  <*> parseDigitStr
  where
    join Nothing a b c = a ++ [b] ++ c
    join (Just x) a b c = (x:a) ++ [b] ++ c

parseDouble :: Parser Double
parseDouble = fmap read parseDoubleString

parseFloat :: Parser Token
parseFloat = applyFloat <$> parseDouble
  where
    applyFloat x = Constant (Float x)

parseBoolean :: Parser Token
parseBoolean = Constant (Boolean True) <$ parseString "True"
  <|> Constant (Boolean False) <$ parseString "False"

parseCharToken :: Parser Token
parseCharToken = Constant . Char
  <$> between
    (parseChar '\'')
    (parseChar '\'')
    (parseAnyChar
     <|> ('\n' <$ parseString "\n")
     <|> ('\r' <$ parseString "\r")
     <|> ('\t' <$ parseString "\t"))

flattenString :: String -> String
flattenString [] = []
flattenString ('\\':'\\':xs) = '\\' : flattenString xs
flattenString ('\\':'n':xs) = '\n' : flattenString xs
flattenString ('\\':'t':xs) = '\t' : flattenString xs
flattenString ('\\':'r':xs) = '\r' : flattenString xs
flattenString (x:xs) = x : flattenString xs

allowedChars :: [Char]
allowedChars = [' ', '!', '\n', '\t'] ++ ['#' .. '~']

parseStringToken :: Parser Token
parseStringToken = Constant . Str . flattenString
  <$> between
    (parseChar '"')
    (parseChar '"')
    (many (parseSomeChar allowedChars))

parseNil :: Parser Token
parseNil = Constant Nil <$ parseString "nil"

parseConst :: Parser Token
parseConst = parseFloat
  <|> parseInteger
  <|> parseBoolean
  <|> parseCharToken
  <|> parseStringToken
  <|> parseNil

parseSymbol :: Parser Token
parseSymbol = applySymbol <$> parseSomeChar asciiLetters
  <*> many (parseSomeChar (asciiLetters ++ ['0' .. '9'] ++ ['-', '_']))
  where
    asciiLetters = ['a' .. 'z'] ++ ['A' .. 'Z']

    applySymbol x xs = Sym (x:xs)

parseBlank :: Parser Token
parseBlank = Blank <$ some (parseSomeChar [' ', '\n', '\t'])

specialCharParsingTable :: [(String, Token)]
specialCharParsingTable =
  [ ("(", OpenParenthesis)
  , (",", Comma)
  , (";", SemiColon)
  , (".", Period)
  , ("[", OpenBracket)
  , ("]", CloseBracket)
  , ("{", OpenBraces)
  , ("}", CloseBraces)
  , (":", Colon)
  , ("?", QuestionMark)
  , ("+", AddSymbol)
  , ("-", SubSymbol)
  , ("*", Binary Times)
  , ("/", Binary Div)
  , ("%", Binary Mod)
  , ("and ", Binary BoolAnd)
  , ("or ", Binary BoolOr)
  , ("==", Binary Equal)
  , ("=", Binary Assign)
  , ("<=", Binary LowerEq)
  , ("<", Binary Lower)
  , (">=", Binary GreaterEq)
  , (">", Binary Greater)
  , ("!=", Binary NotEq)
  , ("not ", Unary BoolNot)]

specialCharParser :: Parser Token
specialCharParser = foldr ((<|>) . f) empty specialCharParsingTable
  where
    f :: (String, Token) -> Parser Token
    f (name, token) = token <$ parseString name

parsePreprocessor :: Parser Token
parsePreprocessor = f <$> parseString "#define"
  <*> parseBlank
  <*> parseSymbol
  <*> (parseBlank <|> pure Blank)
  <*> between (parseChar '(') (parseChar ')') (many noCloseParenthesisToken)
  where
    f _ _ (Sym name) _ tokens = PreProcessor name tokens
    f _ _ _ _ _ = undefined

noCloseParenthesisToken :: Parser Token
noCloseParenthesisToken = parseBlank
  <|> parsePreprocessor
  <|> parseConst
  <|> specialCharParser
  <|> parseSymbol

parseToken :: Parser Token
parseToken = parseBlank
  <|> parsePreprocessor
  <|> parseConst
  <|> specialCharParser
  <|> parseSymbol
  <|> (CloseParenthesis <$ parseString ")")

tokenize :: String -> Either [Token] String
tokenize "" = Left []
tokenize str = case runParser parseToken str of
  Just (Blank, leftovers) -> tokenize leftovers
  Just (tokenHead, leftovers) -> case tokenize leftovers of
    Left tokenTail -> Left (tokenHead:tokenTail)
    Right err      -> Right err
  Nothing -> Right str

