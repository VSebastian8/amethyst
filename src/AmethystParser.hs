module AmethystParser where

import BasicParser
import SyntaxExamples

import Control.Applicative

-- Syntax

-- Basic types
data Move = L | R | N
    deriving Show
data Transition = Transition { getReadSymbol :: Char, getWriteSymbol :: Char, getMove :: Move, getNewState :: String }
    deriving Show
data State = Accept { getStateName :: String } 
           | Reject { getStateName :: String }
           | State  { getStateName :: String, getTransitions :: [Transition], getInitial :: Bool }
    deriving Show
-- Macro Types
data MacroKeyword = Complement String
                  | Intersect [String] 
                  | Reunion [String] 
                  | Chain [String]
                  | Repeat Int String
                  | Move Move Int 
                  | Override Move Int Char
                  | Place String
                  | Shift Move Int
    deriving Show
-- Turing Machines
data Automata = Machine { 
                    getAutomataName :: String, 
                    getStates :: [State], 
                    getComponents :: [(String, String)] }
              | Macro { 
                    getAutomataName :: String, 
                    getKeyword :: MacroKeyword }
    deriving Show
newtype Program = Program { getAutomata :: [Automata] }
    deriving Show

allowedNameSymbols = ['a'..'z'] ++ "0123456789_."
allowedTapeSymbols = ['A'..'Z'] ++ "0123456789" ++ "!@#$%^&*()-+=/?_:"

wordP :: Parser String
wordP = spanP (`elem` allowedNameSymbols)

tapeP :: Parser String
tapeP = spanP (`elem` allowedTapeSymbols)

symbolP :: Parser Char
symbolP = foldr1 (<|>) $ map charP allowedTapeSymbols

moveP :: Parser Move
moveP = selectMove <$> (foldr1 (<|>) $ map charP "LRN")
    where selectMove c = case c of
                            'L' -> L
                            'R' -> R
                            'N' -> N

transitionP :: Parser Transition
transitionP = Transition 
        <$> (ws *> symbolP <* ws <* charP '/') 
        <*> (ws *> symbolP <* ws <* charP ',')
        <*> (ws *> moveP <* ws <* stringP "->")
        <*> (ws *> wordP <* ws <* charP ';')

stateP :: Parser State
stateP = (Reject <$> rejectP) 
     <|> (Accept <$> acceptP) 
     <|> ((makeState True) <$> initialP <*> trP)
     <|> ((makeState False) <$> normalP <*> trP)
    where
        rejectP  =  ws *> stringP "reject" *> ws2 
                    *> stringP "state" *> ws2 
                    *> wordP <* ws <* charP ';'
        acceptP  =  ws *> stringP "accept" *> ws2 
                    *> stringP "state" *> ws2 
                    *> wordP <* ws <* charP ';'
        initialP =  ws *> stringP "initial" *> ws2 
                    *> stringP "state" *> ws2 
                    *> wordP <* ws
        normalP  =  ws *> stringP "state" <* ws
        trP = charP '{' *> some transitionP <* ws <* charP '}'
        makeState :: Bool -> String -> [Transition] -> State
        makeState initial name transitions = State name transitions initial

-- Macro Parsers
complementP :: Parser MacroKeyword
complementP = Complement 
    <$> (stringP "complement" *> ws *> charP '(' *> ws 
         *> wordP <* ws <* charP ')')

intersectP :: Parser MacroKeyword
intersectP = Intersect 
    <$> (stringP "intersect" *> ws *> charP '(' *> ws *>
         sepBy (ws *> charP ',' <* ws) wordP <* ws <* charP ')')

reunionP :: Parser MacroKeyword
reunionP = Reunion 
    <$> (stringP "reunion" *> ws *> charP '(' *> ws *>
         sepBy (ws *> charP ',' <* ws) wordP <* ws <* charP ')')

chainP :: Parser MacroKeyword
chainP = Chain 
    <$> (stringP "chain" *> ws *> charP '(' *> ws *>
         sepBy (ws *> charP ',' <* ws) wordP <* ws <* charP ')')

repeatP :: Parser MacroKeyword
repeatP = Repeat 
    <$> (stringP "repeat" *> ws *> charP '(' *> ws *> numberP <* ws) 
    <*> (charP ',' *> ws *> wordP <* ws <* charP ')')

moveMP :: Parser MacroKeyword
moveMP = Move 
    <$> (stringP "move" *> ws *> charP '(' *> ws *> moveP <* ws) 
    <*> (charP ',' *> ws *> numberP <* ws <* charP ')')

overrideP :: Parser MacroKeyword
overrideP = Override
    <$> (stringP "override" *> ws *> charP '(' *> ws *> moveP <* ws)
    <*> (charP ',' *> ws *> numberP <* ws <* charP ',' <* ws)
    <*> (charP '\'' *> symbolP <* charP '\''  <* ws <* charP ')')

placeP :: Parser MacroKeyword
placeP = Place 
    <$> (stringP "place" *> ws *> charP '(' *> ws *>
         charP '"' *> tapeP <* charP '"' <* ws <* charP ')')

shiftP :: Parser MacroKeyword
shiftP = Shift
    <$> (stringP "shift" *> ws *> charP '(' *> ws *> moveP <* ws)
    <*> (charP ',' *> ws *> numberP <* ws <* charP ')')

macroP :: Parser Automata
macroP = Macro 
    <$> (stringP "automata" *> ws2 *> wordP <* ws <* charP '=' <* ws)
    <*> (complementP <|> intersectP <|> reunionP <|> chainP <|> repeatP
        <|> moveMP <|> overrideP <|> placeP <|> shiftP) <* ws <* charP ';'