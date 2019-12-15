module IntcodeV3 (
    exec
  , mkIntcode
  , runProgram
  , Register
  , State(..)
) where

import qualified Data.Sequence as S
import Data.Sequence((|>))
import Data.Foldable (toList)
import Data.Maybe

type Register = Int
type Position = Int

data Address
    = PositionMode Int
    | ImmediateMode Int
    | RelativeMode Int
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
    | RBS Address
    | HLT
    deriving(Show, Eq)

data State =
    State {
        sId :: Char
      , sPosition :: Position
      , sRelativeBase :: Int
      , sInput :: [Int]
      , sOutput :: [Int]
      , sFinished :: Bool
      , sRegisters :: S.Seq Register
    } deriving(Show, Eq, Ord)

exec :: [Int] -> [Register] -> [Int]
exec input = sOutput . runProgram . mkIntcode 'A' input

mkIntcode :: Char -> [Int] -> [Register] -> State
mkIntcode id input = State id 0 0 input [] False . S.fromList

runProgram :: State -> State
runProgram state =  run state $ toCode (sRegisters state) (sPosition state)
    where
        run state (op, (Just skip)) = maybe state (runProgram . incP skip) $ execOp state op
        run state (op, Nothing) = fromJust $ execOp state op
        incP skip state' = state' { sPosition = (sPosition state') + skip }

registerA :: Address -> State -> Register
registerA addr state = case addr of
    (PositionMode p) -> getReg p $ sRegisters state
    (ImmediateMode p) -> p
    (RelativeMode p) -> getReg (p + sRelativeBase state) $ sRegisters state

positionA :: Address -> State -> Int
positionA (PositionMode a) _ = a
positionA (RelativeMode a) state = a + sRelativeBase state

registerABC :: Address -> Address -> Address -> State -> (Int, Int, Int)
registerABC a b c state = (registerA a state, registerA b state, positionA c state)

getReg :: Position -> S.Seq Register -> Register
getReg n = fromMaybe 0 . S.lookup n

setReg :: Int -> Int -> State -> State
setReg pos val state = state { sRegisters = update (sRegisters state) }
    where update registers
            | pos < 0 = error "index out of bounds"
            | pos >= (S.length registers) = (foldr (\_ r -> r |> 0) registers [1..pos - (S.length registers)]) |> val
            | otherwise = S.update pos val registers

execOp :: State -> InstructionSet -> Maybe State

execOp state (ADD a b c) = op $ registerABC a b c state
    where
        op (a, b, c) = Just $ setReg c (a + b) state

execOp state (MUL a b c) = op $ registerABC a b c state
    where
        op (a, b, c) = Just $ setReg c (a * b) state

execOp state (GET a) = op $ sInput state
    where
        op [] = Nothing
        op (x:xs) = Just $ (setReg (positionA a state) x state) { sInput = xs }

execOp state (PUT a) = op $ registerA a state
    where
        op v = Just $ state { sOutput = v:(sOutput state) }

execOp state (JIT a b) = op (registerA a state) (registerA b state)
    where
        op a' b' = Just $ state { sPosition = if a' /= 0 then b' else (sPosition state) + 3 }

execOp state (JIF a b) = op (registerA a state) (registerA b state)
    where
        op a' b' = Just $ state { sPosition = if a' == 0 then b' else (sPosition state) + 3 }

execOp state (SLT a b c) = op $ registerABC a b c state
    where
        op (a, b, c) = Just $ setReg c (if a < b then 1 else 0) state

execOp state (SEQ a b c) = op $ registerABC a b c state
    where
        op (a, b, c) = Just $ setReg c (if a == b then 1 else 0) state

execOp state (RBS a) = Just $ state { sRelativeBase = (registerA a state) + (sRelativeBase state) }

execOp state HLT = Just $ state { sFinished = True }

toAddress :: S.Seq Register -> Position -> Int -> Address
toAddress registers pos shift = toA $ registers `S.index` (pos + shift)
    where
        toA p = case (registers `S.index` pos) `div` (10 ^ (shift + 1)) `mod` 10 of
            0 -> PositionMode p
            1 -> ImmediateMode p
            2 -> RelativeMode p

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
    9 -> (RBS (toAddress registers pos 1), Just 2)
    99 -> (HLT, Nothing)
