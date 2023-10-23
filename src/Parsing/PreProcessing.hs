module Parsing.PreProcessing (applyPreProcessing) where

import           Types (Token(..))
import qualified Data.Map.Lazy as Map

addDefines :: [Token] -> Map.Map String [Token] -> [Token]
addDefines [] _ = []
addDefines (PreProcessor name tokens:xs) m =
  addDefines xs (Map.insert name tokens m)
addDefines (Sym s:xs) m = case Map.lookup s m of
  Just tokens -> tokens ++ addDefines xs m
  Nothing -> Sym s: addDefines xs m
addDefines (x:xs) m = x:addDefines xs m

applyPreProcessing :: [Token] -> [Token]
applyPreProcessing tokens = addDefines tokens Map.empty
