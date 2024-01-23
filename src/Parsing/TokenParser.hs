{-# LANGUAGE LambdaCase #-}

module Parsing.TokenParser (tokensToAst) where

import           Types
import           Parsing.Parser (second, fuse)
import           Control.Applicative (Alternative(many), (<|>), empty)
import           Safe (findIndexJust)

newtype TokenParser a =
  TokenParser { tokenRunParser :: [Token] -> Maybe (a, [Token]) }

between :: TokenParser a -> TokenParser b -> TokenParser c -> TokenParser c
between a b c = getMiddle <$> a <*> c <*> b
  where
    getMiddle _ x _ = x

instance Functor TokenParser where
  fmap ftc (TokenParser p1) = TokenParser
    $ \str -> case p1 str of
      Just (x, str2) -> Just (ftc x, str2)
      Nothing        -> Nothing

instance Applicative TokenParser where
  pure a = TokenParser $ \str -> Just (a, str)

  (TokenParser pf) <*> (TokenParser p1) = TokenParser
    $ \str1 -> case pf str1 of
      Nothing        -> Nothing
      Just (f, str2) -> case p1 str2 of
        Nothing        -> Nothing
        Just (a, str3) -> Just (f a, str3)

instance Alternative TokenParser where
  (<|>) (TokenParser p1) (TokenParser p2) = TokenParser
    $ \str -> case p1 str of
      Just x  -> Just x
      Nothing -> p2 str

  empty = TokenParser $ const Nothing

parseMaybe :: TokenParser a -> TokenParser (Maybe a)
parseMaybe (TokenParser p1) = TokenParser
  $ \str -> case p1 str of
    Just (res, token) -> Just (Just res, token)
    Nothing           -> Just (Nothing, str)

parseToken :: Token -> TokenParser Token
parseToken token = TokenParser
  $ \case
    (x:xs)
      | x == token -> Just (token, xs)
    _      -> Nothing

parseTokenType :: Token -> TokenParser Ast
parseTokenType (Constant (Integer _)) = TokenParser
  $ \case
    (Constant (Integer x):xs) -> Just (Const (Integer x), xs)
    _ -> Nothing
parseTokenType (Constant (Float _)) = TokenParser
  $ \case
    (Constant (Float x):xs) -> Just (Const (Float x), xs)
    _ -> Nothing
parseTokenType (Constant (Char _)) = TokenParser
  $ \case
    (Constant (Char x):xs) -> Just (Const (Char x), xs)
    _ -> Nothing
parseTokenType (Constant (Boolean _)) = TokenParser
  $ \case
    (Constant (Boolean x):xs) -> Just (Const (Boolean x), xs)
    _ -> Nothing
parseTokenType (Sym _) = TokenParser
  $ \case
    (Sym x:xs) -> Just (Symbol x, xs)
    _          -> Nothing
parseTokenType (Constant (Str _)) = TokenParser
  $ \case
    (Constant (Str x):xs) -> Just (Const (Str x), xs)
    _ -> Nothing
parseTokenType _ = empty

orderedParserList :: [(String, TokenParser Ast)]
orderedParserList =
  [ ("assignment", parseAssign)
  , ("ternary", parseTernaryOperation)
  , ("boolLogic", parseBoolLogic)
  , ("boolNot", parseBoolNot)
  , ("comparisons", parseComparisons)
  , ("addsub", parseAddSub)
  , ("muldiv", parseMulDiv)
  , ("unary", parseUnaryOperation)
  , ("functionCall", parseFunctionCall)
  , ("return", parseReturn)
  , ("subscription", parseSubScription)
  , ("const", parseConst)
  , ("symbol", parseSymbol)
  , ("itemWithParenthesis", parseItemWithParenthesis)]

parserList :: TokenParser Ast
parserList = foldr ((<|>) . snd) empty orderedParserList

removeAbove :: String -> TokenParser Ast
removeAbove "" = parserList
removeAbove str = foldr ((<|>) . snd) empty filteredParsers
  where
    filteredParsers = drop
      (findIndexJust (\x -> fst x == str) orderedParserList)
      orderedParserList

removeAboveEqual :: String -> TokenParser Ast
removeAboveEqual "" = parserList
removeAboveEqual str = foldr ((<|>) . snd) empty filteredParsers
  where
    filteredParsers = drop
      (findIndexJust (\x -> fst x == str) orderedParserList + 1)
      orderedParserList

parseInsideList :: TokenParser a -> TokenParser [a]
parseInsideList p = fuse <$> p <*> many parseOutfix <|> pure []
  where
    parseOutfix = second <$> parseToken Comma <*> p

parseBoolean :: TokenParser Ast
parseBoolean = Const (Boolean True) <$ parseToken (Constant (Boolean True))
  <|> Const (Boolean False) <$ parseToken (Constant (Boolean False))

parseList :: TokenParser Ast
parseList = List
  <$> between
    (parseToken OpenBracket)
    (parseToken CloseBracket)
    (parseInsideList parseItem)

parseInteger :: TokenParser Ast
parseInteger = parseTokenType (Constant (Integer 0))

parseFloat :: TokenParser Ast
parseFloat = parseTokenType (Constant (Float 0))

parseCharAst :: TokenParser Ast
parseCharAst = parseTokenType (Constant (Char '_'))

parseStringAst :: TokenParser Ast
parseStringAst = parseTokenType (Constant (Str ""))

parseInsideDict :: TokenParser [(Ast, Ast)]
parseInsideDict = fuse <$> parseDictItem <*> many parseOutfix <|> pure []
  where
    applyDictItem i1 _ i2 = (i1, i2)

    parseDictItem =
      applyDictItem <$> parseItem <*> parseToken Colon <*> parseItem

    parseOutfix = second <$> parseToken Comma <*> parseDictItem

parseDict :: TokenParser Ast
parseDict = Dict
  <$> between (parseToken OpenBraces) (parseToken CloseBraces) parseInsideDict

parseNil :: TokenParser Ast
parseNil = Const Nil <$ parseToken (Constant Nil)

parseConst :: TokenParser Ast
parseConst = parseFloat
  <|> parseInteger
  <|> parseList
  <|> parseBoolean
  <|> parseCharAst
  <|> parseStringAst
  <|> parseNil
  <|> parseDict

parseSymbol :: TokenParser Ast
parseSymbol = parseTokenType (Sym "")

parseItemWithParenthesis :: TokenParser Ast
parseItemWithParenthesis =
  between (parseToken OpenParenthesis) (parseToken CloseParenthesis) parseItem

parseItem :: TokenParser Ast
parseItem = parserList

parseForStuff :: TokenParser (Ast, Ast, Ast)
parseForStuff = between
  (parseToken OpenParenthesis)
  (parseToken CloseParenthesis)
  (f <$> p <*> parseToken SemiColon <*> p <*> parseToken SemiColon <*> p)
  where
    f a _ b _ c = (a, b, c)

    p = parseItem <|> pure None

parseFor :: TokenParser Ast
parseFor =
  applyWhileFor <$> parseToken (Sym "for") <*> parseForStuff <*> parseBlock
  where
    applyWhileFor _ (ini, cond, inc) = ForBlock ini cond inc

parseWhile :: TokenParser Ast
parseWhile = WhileBlock <$ parseToken (Sym "while")
  <*> parseItemWithParenthesis
  <*> parseBlock

parseElseIf :: TokenParser [IfType]
parseElseIf = many
  (applyElseIf <$> parseToken (Sym "elif")
   <*> parseItemWithParenthesis
   <*> parseBlock)
  where
    applyElseIf _ params block = (params, block)

parseIf :: TokenParser Ast
parseIf = IfBlock
  <$> (applyIf <$> parseToken (Sym "if")
       <*> parseItemWithParenthesis
       <*> parseBlock)
  <*> parseElseIf
  <*> parseMaybe (second <$> parseToken (Sym "else") <*> parseBlock)
  where
    applyIf _ x y = (x, y)

parseReturn :: TokenParser Ast
parseReturn = applyReturn <$> parseToken (Sym "return")
  <*> (parseItem <|> pure (Const Nil))
  where
    applyReturn _ = Return

parseAst :: TokenParser Ast
parseAst = (extractFirst <$> (parseItem <|> pure None) <*> parseToken SemiColon)
  <|> parseIf
  <|> parseWhile
  <|> parseFor
  <|> parseFunctionDef
  where
    extractFirst x _ = x

parseBlock :: TokenParser [Ast]
parseBlock =
  between (parseToken OpenBraces) (parseToken CloseBraces) (many parseAst)

parseUnary :: String -> TokenParser UnaryOperator -> TokenParser Ast
parseUnary name parseOps = UnaryOp <$> parseOps <*> removeAboveEqual name

parseUnaryOperation :: TokenParser Ast
parseUnaryOperation = parseUnary
  "unary"
  (BoolNot <$ parseToken (Unary BoolNot) <|> Negative <$ parseToken SubSymbol)

parseBinary :: String -> TokenParser BinaryOperator -> TokenParser Ast
parseBinary string parseOps =
  applyBin <$> parseNoSelf <*> parseOps <*> parseWithSelf
  where
    parseWithSelf = removeAbove string

    parseNoSelf = removeAboveEqual string

    applyBin x Sub (BinaryOp Sub x' y') = BinaryOp Sub x (BinaryOp Add x' y')
    applyBin x Sub (BinaryOp Add x' y') = BinaryOp Sub x (BinaryOp Sub x' y')
    applyBin x op y = BinaryOp op x y

parseAssign :: TokenParser Ast
parseAssign = parseBinary "assignment" (Assign <$ parseToken (Binary Assign))

parseSubCall :: TokenParser Ast
parseSubCall =
  applyFunction <$> parseSymbol <*> parseToken Period <*> parseFunctionCall
  where
    applyFunction parent _ (FunctionCall name args) =
      FunctionCall name (parent:args)
    applyFunction _ _ _ = None

parseSubIndex :: TokenParser Ast
parseSubIndex = applySub <$> parseSymbol
  <*> between (parseToken OpenBracket) (parseToken CloseBracket) parseItem
  where
    applySub (Symbol var) index = IndexOf var index
    applySub _ _ = None

parseSubScription :: TokenParser Ast
parseSubScription = parseSubCall <|> parseSubIndex

parseComparisons :: TokenParser Ast
parseComparisons = parseBinary
  "comparisons"
  (Equal <$ parseToken (Binary Equal)
   <|> LowerEq <$ parseToken (Binary LowerEq)
   <|> Lower <$ parseToken (Binary Lower)
   <|> GreaterEq <$ parseToken (Binary GreaterEq)
   <|> Greater <$ parseToken (Binary Greater)
   <|> NotEq <$ parseToken (Binary NotEq))

parseAddSub :: TokenParser Ast
parseAddSub = parseBinary "addsub" parseOps
  where
    parseOps = Add <$ parseToken AddSymbol <|> Sub <$ parseToken SubSymbol

parseMulDiv :: TokenParser Ast
parseMulDiv = parseBinary "muldiv" parseOps
  where
    parseOps = Times <$ parseToken (Binary Times)
      <|> Div <$ parseToken (Binary Div)
      <|> Mod <$ parseToken (Binary Mod)

parseBoolLogic :: TokenParser Ast
parseBoolLogic = parseBinary
  "boolLogic"
  (BoolAnd <$ parseToken (Binary BoolAnd)
   <|> BoolOr <$ parseToken (Binary BoolOr))

parseBoolNot :: TokenParser Ast
parseBoolNot = parseUnary "boolNot" (BoolNot <$ parseToken (Unary BoolNot))

parseSymbolString :: TokenParser String
parseSymbolString = symToString <$> parseTokenType (Sym "")
  where
    symToString (Symbol x) = x
    symToString _ = undefined

parseTernaryOperation :: TokenParser Ast
parseTernaryOperation = applyTernaryOperation <$> removeAboveEqual "ternary"
  <*> parseToken QuestionMark
  <*> parseItem
  <*> parseToken Colon
  <*> parseItem
  where
    applyTernaryOperation bool _ true _ = TernaryOp TernaryGate bool true

parseFunctionCall :: TokenParser Ast
parseFunctionCall = FunctionCall <$> parseSymbolString
  <*> between
    (parseToken OpenParenthesis)
    (parseToken CloseParenthesis)
    (parseInsideList parseItem)

parseFunctionDef :: TokenParser Ast
parseFunctionDef = applyDef <$> parseToken (Sym "function")
  <*> parseSymbolString
  <*> between
    (parseToken OpenParenthesis)
    (parseToken CloseParenthesis)
    (parseInsideList parseSymbolString)
  <*> parseBlock
  where
    applyDef _ = FunctionDefinition

tokensToAst :: [Token] -> Either [Ast] String
tokensToAst [] = Left []
tokensToAst tokens = case tokenRunParser parseAst tokens of
  Just (tokenHead, leftovers) -> case tokensToAst leftovers of
    Left tokenTail -> Left (tokenHead:tokenTail)
    Right err      -> Right err
  Nothing -> Right "Invalid syntax"
