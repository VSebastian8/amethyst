module AmethystSyntax where

-- Basic types
data Move = L | R | N
  deriving (Show)
data Transition = Transition {getReadSymbol :: Char, getWriteSymbol :: Char, getMove :: Move, getNewState :: String}
  deriving (Show)
data State
  = Accept {getStateName :: String}
  | Reject {getStateName :: String}
  | State {getStateName :: String, getTransitions :: [Transition], getInitial :: Bool}
  deriving (Show)

-- Macro Types
data MacroKeyword
  = Complement String
  | Intersect [String]
  | Reunion [String]
  | Chain [String]
  | Repeat Int String
  | Move Move Int
  | Override Move Int Char
  | Place String
  | Shift Move Int
  deriving (Show)

-- Turing Machines
data Automata
  = Machine {getAutomataName :: String, getComponents :: [(String, String)], getStates :: [State]}
  | Macro {getAutomataName :: String, getKeyword :: MacroKeyword}
  deriving (Show)
newtype Program = Program {getAutomata :: [Automata]}
  deriving (Show)

-- Alphabet configuration
allowedNameSymbols :: [Char]
allowedNameSymbols = ['a' .. 'z'] ++ "0123456789_."

allowedTapeSymbols :: [Char]
allowedTapeSymbols = ['A' .. 'Z'] ++ "0123456789" ++ "!@#$%^&*()-+=/?_:"
