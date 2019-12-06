module IntcodeV2 (
    exec
  , Register
) where

import qualified Data.Sequence as S
import Data.Foldable (toList)

type Register = Int
type Position = Int

data Address
    = PositionMode Int
    | ImmediateMode Int
    deriving(Show, Eq)

data InstructionSet
    = ADD Address Address Address
    | MUL Address Address Address
    | GET Address
    | PUT Address
    | JIT Address Address
    | JIF Address Address
    | SLT Address Address Address
    | SEQ Address Address Address
    | HLT
    deriving(Show, Eq)

data State =
    State {
        sPosition :: Position
      , sInput :: [Int]
      , sOutput :: [Int]
      , sRegisters :: S.Seq Register
    } deriving(Show, Eq)

exec :: [Int] -> [Register] -> [Int]
exec input = sOutput . runProgram . State 0 input [] . S.fromList

runProgram :: State -> State
runProgram state =  run state $ toCode (sRegisters state) (sPosition state)
    where
        run state (op, (Just skip)) = runProgram . incP skip $ execOp state op
        run state (op, Nothing) = execOp state op
        incP skip state' = state' { sPosition = (sPosition state') + skip }

registerA :: Address -> State -> Int
registerA addr state = case addr of
    (PositionMode a) -> (sRegisters state) `S.index` a
    (ImmediateMode a) -> a

registerABC :: Address -> Address -> Address -> State -> (Int, Int, Int)
registerABC a b (PositionMode c) state = (registerA a state, registerA b state, c)

setReg :: Int -> Int -> State -> State
setReg pos val state = state { sRegisters = S.update pos val (sRegisters state) }

execOp :: State -> InstructionSet -> State

execOp state (ADD a b c) = op $ registerABC a b c state
    where
        op (a, b, c) = setReg c (a + b) state

execOp state (MUL a b c) = op $ registerABC a b c state
    where
        op (a, b, c) = setReg c (a * b) state

execOp state (GET (PositionMode a)) = op $ sInput state
    where
        op (x:xs) = (setReg a x state) { sInput = xs }

execOp state (PUT a) = op $ registerA a state
    where
        op v = state { sOutput = v:(sOutput state) }

execOp state (JIT a b) = op (registerA a state) (registerA b state)
    where
        op a' b' = state { sPosition = if a' /= 0 then b' else (sPosition state) + 3 }

execOp state (JIF a b) = op (registerA a state) (registerA b state)
    where
        op a' b' = state { sPosition = if a' == 0 then b' else (sPosition state) + 3 }

execOp state (SLT a b c) = op $ registerABC a b c state
    where
        op (a, b, c) = setReg c (if a < b then 1 else 0) state

execOp state (SEQ a b c) = op $ registerABC a b c state
    where
        op (a, b, c) = setReg c (if a == b then 1 else 0) state

execOp state HLT = state

toAddress :: S.Seq Register -> Position -> Int -> Address
toAddress registers pos shift = toA $ registers `S.index` (pos + shift)
    where
        toA p = case (registers `S.index` pos) `div` (10 ^ (shift + 1)) `mod` 10 of
            0 -> PositionMode p
            1 -> ImmediateMode p

toCode :: S.Seq Register -> Position -> (InstructionSet, Maybe Int)
toCode registers pos = case (registers `S.index` pos `mod` 100) of
    1 -> (ADD (toAddress registers pos 1) (toAddress registers pos 2) (toAddress registers pos 3), Just 4)
    2 -> (MUL (toAddress registers pos 1) (toAddress registers pos 2) (toAddress registers pos 3), Just 4)
    3 -> (GET (toAddress registers pos 1), Just 2)
    4 -> (PUT (toAddress registers pos 1), Just 2)
    5 -> (JIT (toAddress registers pos 1) (toAddress registers pos 2), Just 0)
    6 -> (JIF (toAddress registers pos 1) (toAddress registers pos 2), Just 0)
    7 -> (SLT (toAddress registers pos 1) (toAddress registers pos 2) (toAddress registers pos 3), Just 4)
    8 -> (SEQ (toAddress registers pos 1) (toAddress registers pos 2) (toAddress registers pos 3), Just 4)
    99 -> (HLT, Nothing)
