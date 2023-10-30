module Instructions.AstToInstructions (astListToInstructions) where

import           Types (Ast(..), UnaryOperator(..), BinaryOperator(..)
                      , Instruction(..), TernaryOperator(TernaryGate), IfType
                      , Value(Boolean))
import qualified Types as T (Instruction(..))
import           Basement.Compat.Base (Int64)
import qualified Data.Bifunctor

unaryToInst :: UnaryOperator -> Instruction
unaryToInst BoolNot = NOT
unaryToInst Negative = NEGATIVE

binaryToInst :: BinaryOperator -> Instruction
binaryToInst Add = ADD
binaryToInst Sub = SUB
binaryToInst Times = MUL
binaryToInst Div = DIV
binaryToInst Mod = MOD
binaryToInst Assign = Set
binaryToInst Equal = T.EQ
binaryToInst Lower = T.LT
binaryToInst LowerEq = LET
binaryToInst Greater = T.GT
binaryToInst GreaterEq = GET
binaryToInst NotEq = NEQ
binaryToInst BoolAnd = AND
binaryToInst BoolOr = OR

fi :: Int -> Int64
fi = fromIntegral

whileToInst :: Ast -> [Ast] -> [Instruction]
whileToInst condition block =
  instCond ++ [JIF toEnd] ++ instBlock ++ [Jump toStart]
  where
    instCond = astToInst condition

    instBlock = astListToInstructions block

    toEnd = fi (length instBlock + 1)

    toStart = fi $ -(length instBlock + length instCond + 1 + 1)

listToInst :: [Ast] -> [Instruction]
listToInst elems = concatMap astToInst elems ++ [LIST (fi (length elems))]

ifToInst :: [Instruction] -> [Instruction] -> Int64 -> Int64 -> [Instruction]
ifToInst cond block endPos curPos =
  cond ++ JIF toEndOfBlock:block ++ [Jump toEnd]
  where
    toEndOfBlock = fi (length block) + 1

    toEnd = endPos - (curPos + fi (length cond) + 2 + fi (length block))

ifsToInstLogic
  :: [([Instruction], [Instruction])] -> Int64 -> Int64 -> [Instruction]
ifsToInstLogic [] _ _ = []
ifsToInstLogic ((cond, block):xs) endPos curPos =
  ifToInst cond block endPos curPos
  ++ ifsToInstLogic xs endPos (curPos + sizeOfItem)
  where
    sizeOfItem = fi (length cond) + 2 + fi (length block)

ifsToInst :: [IfType] -> Maybe [Ast] -> [Instruction]
ifsToInst ifs Nothing = ifsToInstLogic pairs sizeOfInsts 0
  where
    pairs = map (Data.Bifunctor.bimap astToInst astListToInstructions) ifs

    sizeOfInsts = fi
      $ length pairs * 2
      + sum (map (\x -> length (fst x) + length (snd x)) pairs)
ifsToInst ifs (Just block) =
  ifsToInst (ifs ++ [(Const (Boolean True), block)]) Nothing

astToInst :: Ast -> [Instruction]
astToInst (Symbol s) = [PushSymbol s]
astToInst (Const c) = [Push c]
astToInst (List x) = listToInst (reverse x)
astToInst (UnaryOp op eval) = astToInst eval ++ [unaryToInst op]
astToInst (BinaryOp op n1 n2) =
  astToInst n2 ++ astToInst n1 ++ [binaryToInst op]
astToInst (TernaryOp _ n1 n2 n3) = astToInst (IfBlock (n1, [n2]) [] (Just [n3]))
astToInst (Block block) = astListToInstructions block
astToInst (FunctionCall name args) =
  concatMap astToInst (reverse args) ++ [PushSymbol name] ++ [Call]
astToInst (FunctionDefinition name args block) =
  Function name args:astListToInstructions block
astToInst (IfBlock x y z) = ifsToInst (x:y) z
astToInst (WhileBlock cond block) = whileToInst cond block
astToInst (ForBlock ini cond inc block) = astToInst ini
  ++ whileToInst cond (block ++ [inc])
astToInst (Return ast) = astToInst ast ++ [RET]
astToInst (IndexOf symbol index) = astToInst index ++ [INDEX symbol]
astToInst None = []
astToInst (Dict _) = undefined

astListToInstructions :: [Ast] -> [Instruction]
astListToInstructions = concatMap astToInst
