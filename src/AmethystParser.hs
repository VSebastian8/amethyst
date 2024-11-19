module AmethystParser where

import Control.Applicative
import SyntaxExamples

-- Syntax

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

allowedNameSymbols = ['a'..'z'] ++ "0123456789_."
allowedTapeSymbols = ['A'..'Z'] ++ "0123456789" ++ "!@#$%^&*()-+=/?_:"

-- Parser instances

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
    fmap f (Parser p) = Parser (\input -> do
                        (input', x) <- p input
                        Just (input', f x))

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
                        (input1, f) <- p1 input
                        (input2, x) <- p2 input1
                        Just (input2, f x)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> do
                        p1 input <|> p2 input

-- Parser combinators

charP :: Char -> Parser Char
charP x = Parser f
    where
        f [] = Nothing
        f (y:ys)
            |y == x = Just (ys, x)
            |otherwise = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> 
    let (token, rest) = span f input
    in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
                (input', xs) <- p input
                if null xs 
                    then Nothing
                    else Just (input', xs)

ws :: Parser String
ws = spanP (\c -> c == ' ' || c == '\n')

-- At least one white space
ws2 :: Parser String 
ws2 = notNull ws

wordP :: Parser String
wordP = spanP (`elem` allowedNameSymbols)

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