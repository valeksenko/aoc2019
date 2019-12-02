module D02 (
    exec
  , Register
) where

import qualified Data.Sequence as S
import Data.Foldable (toList)

type Register = Int
type Address = (Int, Int, Int)

data InstructionSet
    = ADD Address
    | MUL Address
    | HLT
    deriving(Show, Eq)

exec :: [Register] -> [Register]
exec = toList . runProgram 0 . S.fromList

runProgram :: Int -> S.Seq Register -> S.Seq Register
runProgram pos registers =  run $ toCode registers pos
    where
        run (op, (Just nextPos)) = runProgram nextPos $ execOp registers op
        run (op, Nothing) = execOp registers op

registerAB :: Address -> S.Seq Register -> Address
registerAB (a, b, c) registers = (registers `S.index` a, registers `S.index` b, c)

setC :: Int -> Register -> S.Seq Register -> S.Seq Register
setC = S.update

execOp :: S.Seq Register -> InstructionSet -> S.Seq Register

execOp registers (ADD addr) = op $ registerAB addr registers
    where
        op (a, b, c) = setC c (a + b) registers

execOp registers (MUL addr) = op $ registerAB addr registers
    where
        op (a, b, c) = setC c (a * b) registers

execOp registers HLT = registers

toAddress :: S.Seq Register -> Int -> Address
toAddress registers pos = (registers `S.index` (pos + 1), registers `S.index` (pos + 2), registers `S.index` (pos + 3))

toCode :: S.Seq Register -> Int -> (InstructionSet, Maybe Int)
toCode registers pos = case (registers `S.index` pos) of
    1 -> (ADD $ toAddress registers pos, Just $ pos + 4)
    2 -> (MUL $ toAddress registers pos, Just $ pos + 4)
    99 -> (HLT, Nothing)
