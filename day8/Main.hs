module Main where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Maybe (fromJust)

data State = State {accumulator :: Int, pc :: Int, visitedIndexes :: Set Int}
    deriving (Show, Eq, Ord)

initState :: State
initState = State 0 0 S.empty

data Instruction = NOP Int | ACC Int | JMP Int | TERMINATE
    deriving (Show, Eq)

type Program = Vector Instruction

parseInstruction :: String -> Instruction
parseInstruction str = 
    case take 3 str of
        "nop" -> NOP (read . drop 4 $ str)
        "acc" -> ACC (read . drop 4 $ str)
        "jmp" -> JMP (read . drop 4 $ str)
        _     -> NOP 0

parseProgram :: String -> Program
parseProgram s = V.snoc (V.fromList $ parseInstruction <$> lines s) TERMINATE

possibles :: Program -> Vector Program
possibles program = swap program <$> indices
    where indices = V.findIndices noporjmp program
          noporjmp (NOP _) = True
          noporjmp (JMP _) = True
          noporjmp  _       = False
          swap :: Program -> Int -> Program
          swap p i = case p ! i of
              NOP n -> p // [(i, JMP n)]
              JMP n -> p // [(i, NOP n)]

mbRun :: Program -> State -> Maybe (State, Program)
mbRun p (State a pc v) | S.member pc v = Nothing
                       | otherwise =
        case p ! pc of
            NOP _ -> mbRun p (State a (pc + 1) (S.insert pc v))
            ACC n -> mbRun p (State (a + n) (pc + 1) (S.insert pc v))
            JMP n -> mbRun p (State a (pc + n) (S.insert pc v))
            TERMINATE -> Just (State a pc v, p)

runOnPossibles :: Program -> Vector State
runOnPossibles p = (fst . fromJust <$>) . V.filter (/= Nothing) $ flip mbRun initState <$> possibles p

run :: Program -> State -> State
run p (State a pc v) | S.member pc v = State a pc v
                     | otherwise =  
        case p ! pc of
            NOP _ -> run p (State a (pc + 1) (S.insert pc v))
            ACC n -> run p (State (a + n) (pc + 1) (S.insert pc v))
            JMP n -> run p (State a (pc + n) (S.insert pc v))
            TERMINATE -> State a pc v

main :: IO ()
main = do
    file <- readFile "input.txt"
    let program = parseProgram file
    print (accumulator $ run program initState)
    print (runOnPossibles program)