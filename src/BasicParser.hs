module BasicParser where

import Control.Applicative
import AmethystSyntax


newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

-- Parser instances
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

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

ws :: Parser String
ws = spanP (\c -> c == ' ' || c == '\n')

-- At least one white space
ws2 :: Parser String 
ws2 = notNull ws

numberP :: Parser Int
numberP = read <$> spanP (`elem` "0123456789")

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
        rejectP  =  stringP "reject" *> ws2 
                    *> stringP "state" *> ws2 
                    *> wordP <* ws <* charP ';'
        acceptP  =  stringP "accept" *> ws2 
                    *> stringP "state" *> ws2 
                    *> wordP <* ws <* charP ';'
        initialP =  stringP "initial" *> ws2 
                    *> stringP "state" *> ws2 
                    *> wordP <* ws
        normalP  =  stringP "state" *> ws 
                    *> wordP <* ws
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

machineP :: Parser Automata
machineP = Machine
    <$> (stringP "automata" *> ws2 *> wordP <* ws)
    <*> (charP '(' *> sepBy comma pair <* charP ')' <* ws)
    <*> (charP '{' *> some (ws *> stateP <* ws) <* charP '}')
    where 
        comma = ws *> charP ',' <* ws
        pair :: Parser (String, String)
        pair = (\s1 s2 -> (s1, s2)) <$> wordP <*> (ws2 *> wordP)

automataP :: Parser Automata
automataP = macroP <|> machineP

programP :: Parser Program
programP = Program <$> many (ws *> automataP <* ws)
