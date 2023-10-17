module Instructions.AstToInstructions (astListToInstructions) where

import           Types (Ast(..), UnaryOperator(..), BinaryOperator(..)
                      , Instruction(..), TernaryOperator(TernaryGate), IfType)
import qualified Types as T (Instruction(..))

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

ternaryToInst :: TernaryOperator -> Instruction
ternaryToInst TernaryGate = TERNARY

whileToInst :: Ast -> [Ast] -> [Instruction]
whileToInst condition block =
  instCond ++ [JIF toEnd] ++ instBlock ++ [Jump toStart]
  where
    instCond = astToInst condition

    instBlock = astListToInstructions block

    toEnd = fromIntegral (length instBlock + 1)

    toStart = fromIntegral $ -(length instBlock + length instCond + 1 + 1)

listToInst :: [Ast] -> [Instruction]
listToInst elems = concatMap astToInst elems
  ++ [LIST (fromIntegral (length elems))]

ifToInst :: [IfType] -> Maybe [Ast] -> [Instruction]
ifToInst = undefined

astToInst :: Ast -> [Instruction]
astToInst (Symbol s) = [PushSymbol s]
astToInst (Const c) = [Push c]
astToInst (List x) = listToInst x
astToInst (UnaryOp op eval) = astToInst eval ++ [unaryToInst op]
astToInst (BinaryOp op n1 n2) =
  astToInst n2 ++ astToInst n1 ++ [binaryToInst op]
astToInst (TernaryOp op n1 n2 n3) =
  astToInst n3 ++ astToInst n2 ++ astToInst n1 ++ [ternaryToInst op]
astToInst (Block block) = astListToInstructions block
astToInst (FunctionCall name args) =
  concatMap astToInst (reverse args) ++ [PushSymbol name] ++ [Call]
astToInst (FunctionDefinition name args block) =
  Function name args:astListToInstructions block
astToInst (IfBlock x y z) = ifToInst (x:y) z
astToInst (WhileBlock cond block) = whileToInst cond block
astToInst (ForBlock ini cond inc block) = astToInst ini ++ whileToInst cond (block ++ [inc])
astToInst (Return ast) = astToInst ast ++ [RET]
astToInst (IndexOf symbol index) = astToInst index ++ [INDEX symbol]
astToInst None = []
astToInst (Dict _) = undefined

astListToInstructions :: [Ast] -> [Instruction]
astListToInstructions = concatMap astToInst
