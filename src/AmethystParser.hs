module AmethystParser where

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

{- Syntax example

automata not(){
    initial state q0 {
        B / B , R -> q0;
        1 / 0 , L -> q1;
        0 / 1 , L -> q1;
    }
    accept state q1;
}

automata main (not n1, not n2) {
    initial state q0 {
        B/B,N-> n1.q0; _/B,R->qrej;
    }
    state n1.q1 {_/_,N->n2.q0;}
    state n2.q1 {
        _ / _ , N -> qacc;
    }
    reject state qrej; accept state qacc
}
-}

{- Syntax Tree:

Program [
    Automata "not" [
        State "q0" [
            Transition 'B' 'B' R "q0",
            Transition '1' '0' L "q1",
            Transition '0' '1' L "q1"
            ] 
            True, 
        Accept "q1"
        ] 
        [],
    Automata "main" [
        State "q0" [
            Transition 'B' 'B' N "n1.q0",
            Transition '_' 'B' R "qrej"
            ]
            True,
        State "n1.q1" [Transition '_' '_' N "n2.q0"] False
        State "n2.q1" [Transition '_' '_' N "qacc"] False
        Reject "qrej",
        Accept "qacc"
        ]
        [("n1", "not"), ("n2", "not")]
]
-}