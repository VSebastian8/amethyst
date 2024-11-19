module AmethystParser where
import SyntaxExamples

data Move = L | R | N
    deriving Show
data Transition = Transition { getReadSymbol :: Char, getWriteSymbol :: Char, getMove :: Move, getNewState :: String }
    deriving Show
data State = Accept { getStateName :: String } 
           | Reject { getStateName :: String }
           | State  { getStateName :: String, getTransitions :: [Transition], getInitial :: Bool }
    deriving Show
data Automata = Automata { getAutomataName :: String, getStates :: [State], getComponents :: [(String, String)] }
    deriving Show
data Program = Program { getAutomata :: [Automata] }
    deriving Show


