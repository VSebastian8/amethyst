module AmethystSyntax where

data Info 
  = Info {getLine :: Int, getCol :: Int}
  deriving (Show)

data Move = L | R | N
  deriving (Show)

data Transition 
  = Transition {getReadSymbol :: Char, getWriteSymbol :: Char, getMoveSymbol :: Move, getNewState :: String, getTransitionInfo :: Info}
  deriving (Show)

data StateType 
  = Accept
  | Reject
  | Normal {getInitial :: Bool, getTransitions :: [Transition]}
  deriving (Show)

data State 
  = State {getComponentName :: Maybe String, getStateName :: String, getType :: StateType, getStateInfo :: Info}
  deriving (Show)

data Component
  = Component {getPackageName :: Maybe String, getModuleName :: Maybe String, getAutomatonName :: String, getComponentInfo :: Info}
  deriving (Show)

data Machine
  = Machine {getComponents :: [(Component, String)], getStates :: [State]}
  deriving (Show)

data Macro
  = Complement {getAutomaton :: Component}
  | Intersect {getAutomata :: [Component]}
  | Reunion {getAutomata :: [Component]}
  | Chain {getAutomata :: [Component]}
  | Repeat {getAutomaton :: Component, getNumber :: Int}
  | Move {getMove :: Move, getNumber :: Int}
  | Override {getMove :: Move, getNumber :: Int, getSymbol :: Char}
  | Place {getSequence :: String}
  | Shift {getMove :: Move, getNumber :: Int}
  deriving (Show)

data Automaton
  = Macro {getName :: String, getMacro :: Macro, getInfo :: Info}
  | Mach  {getName :: String, getMachine :: Machine, getInfo :: Info}
  deriving(Show)

newtype Program 
  = Program {getAutomataList :: [Automaton]}
  deriving(Show)

breakSymbols, nameSymbols, tapeSymbols :: [Char]
nameSymbols = ['0' .. '9'] ++ ['a' .. 'z'] ++ "_"
tapeSymbols = ['0' .. '9'] ++ ['A' .. 'Z'] ++ "_@!#$%^&*[]-+=?:"
breakSymbols = "(){};=, \n\t\"\'"