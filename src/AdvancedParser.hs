{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use traverse" #-}
module AdvancedParser where

import Control.Applicative
import AmethystSyntax

newtype Error = Error {getErr :: String}
data Leftover = Leftover {getRest :: String, getLine :: Int, getColumn :: Int}
newtype Parser a = Parser {runParser :: Leftover -> Maybe (Leftover, Either Error a)}

instance Show Error where
    show (Error e) = "Error: " ++ e
instance Show Leftover where
    show (Leftover str line col) = "[rest: \"" ++ str
        ++ "\" at line " ++ show line ++ ", column " ++ show col ++ "]"

-- Parser instances
instance Functor Parser where
    fmap f (Parser p) = Parser $ 
        \input -> do
            (input', res) <- p input
            case res of
                Left err -> Just (input', Left err)
                Right el -> Just (input', Right $ f el)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, Right x)
    (Parser p1) <*> (Parser p2) = Parser $ 
        \input -> do
            (input', res) <- p1 input
            case res of
                Left err -> Just (input', Left err)
                Right f  -> do
                    (input'', res') <- p2 input'
                    case res' of
                        Left err -> Just (input'', Left err)
                        Right el -> Just (input'', Right $ f el)


instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ 
        \input -> do
            p1 input <|> p2 input

-- Parser combinators - result or nothing
nextPos :: Leftover -> Char -> Leftover
nextPos (Leftover [] _ _) _ = Leftover  "" (-1) (-1)
nextPos (Leftover (_:ys) line col) x
    |x == '\n' = Leftover ys (line + 1) 0
    |otherwise = Leftover ys line (col + 1)

charP :: Char -> Parser Char
charP x = Parser f
    where
        f (Leftover [] _ _) = Nothing
        f rest@(Leftover (y:_) _ _)
            |y == x = Just (nextPos rest x, Right x)
            |otherwise = Nothing

condCharP :: (Char -> Bool) -> Parser Char
condCharP cond = Parser f
    where
        f (Leftover [] _ _) = Nothing
        f rest@(Leftover (y:_) _ _)
            |cond y = Just (nextPos rest y, Right y)
            |otherwise = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f = many (condCharP f)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
                (input', Right el) <- p input
                if null el
                    then Nothing
                    else Just (input', Right el)

condition :: ([a] -> Bool) -> Parser [a] -> Parser [a]
condition f (Parser p) = Parser $ \input -> do
                (input', Right el) <- p input
                if f el
                    then Just (input', Right el)
                    else Nothing

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

ws :: Parser String
ws = spanP (\c -> c == ' ' || c == '\n')

ws2 :: Parser String
ws2 = notNull ws

numberP :: Parser Int
numberP = read <$> notNull (spanP (`elem` "0123456789"))

literalP :: Parser String
literalP = spanP (\c ->
    all ($c) [not . (`elem` "(){};= \n"), (`elem` allowedNameSymbols ++ allowedTapeSymbols)])

wordP :: Parser String
wordP = spanP (`elem` allowedNameSymbols)

tapeP :: Parser String
tapeP = spanP (`elem` allowedTapeSymbols)

symbolP :: Parser Char
symbolP = foldr1 (<|>) $ map charP allowedTapeSymbols

moveP :: Parser Move
moveP = L <$ charP 'L'
    <|> R <$ charP 'R'
    <|> N <$ charP 'N'

-- Parser combinators - result or error
look :: Leftover -> (Leftover, Error)
look l@(Leftover [] _ _) = (l, Error "no more input")
look (Leftover (y:ys) line col) = (Leftover ys line col, Error $ "'" ++ [y] ++ "'")

lookLit :: Leftover -> (Leftover, Error)
lookLit l@(Leftover [] _ _) = (l, Error "")
lookLit l@(Leftover (y : ys) line col) =
    if all ($y) [not . (`elem` "(){};= \n"), (`elem` allowedNameSymbols ++ allowedTapeSymbols)]
        then
            let (fin, Error rest) = lookLit (Leftover ys line col)
             in (fin, Error $ y : rest)
        else (l, Error "")

makeError :: String -> (Leftover, Error) -> (Leftover, Either Error b)
makeError err (leftover, Error cause) = (leftover, Left $ Error $ err ++ cause)

charPE :: Char -> Parser Char
charPE x = charP x
    <|> Parser (Just . makeError ("expected '" ++ [x] ++ "' - found ") . look)

notNullE :: String -> Parser [a] -> Parser [a]
notNullE msg (Parser p) = Parser $ \input -> do
    (input', res) <- p input
    case res of
        Left err -> Just (input', Left err)
        Right el ->
            if null el
                then Just (input', Left $ Error msg)
                else Just (input', Right el)

-- Check that the first character of the input isn't x, it doesn't consume x
notCharP :: Char -> Parser Char
notCharP x = Parser lookC
  where
    lookC (Leftover [] _ _) = Nothing
    lookC l@(Leftover (y : _) _ _) =
        if y == x
            then Nothing
            else Just (l, Right ' ')

numberPE :: Parser Int
numberPE = numberP
    <|> Parser (Just . makeError "expected number - found " . lookLit)

symbolPE :: Parser Char
symbolPE = symbolP
    <|> Parser (Just . makeError "expected tape symbol - found " . look)

wordPE :: Parser String
wordPE = condition (all (`elem` allowedNameSymbols)) literalP
    <|> Parser (Just . makeError "forbidden symbol in word - " . lookLit)

tapePE :: Parser String
tapePE = condition (all (`elem` allowedTapeSymbols)) literalP
    <|> Parser (Just . makeError "forbidden tape in sequence - " . lookLit)

movePE :: Parser Move
movePE = moveP
    <|> Parser (Just . makeError "expected move symbol - found " . look)

transitionPE :: Parser Transition
transitionPE = Transition
    <$> (ws *> notCharP '}' *> symbolPE <* ws <* charPE '/')
    <*> (ws *> symbolPE <* ws <* charPE ',')
    <*> (ws *> movePE <* ws <* charPE '-' <* charPE '>')
    <*> (ws *> notNullE "expected new state" wordPE <* ws <* charPE ';')

wsE :: Parser String
wsE = some (charP ' ')
    <|> Parser (Just . makeError "expected space - found " . look)

commentPE :: Parser String
commentPE = (charP '-' *> charP '-' *> spanP (/= '\n') <* charP '\n')
    <|> (charP '{' *> charP '-' *> spanP (`notElem` "-}") <* charPE '-' <* charPE '}')

commPE :: Parser String
commPE = ws *> (concat <$> many (commentPE <* ws))

stringPE :: String -> Parser String
stringPE str = stringP str
    <|> Parser (Just . makeError ("expected \"" ++ str ++ "\" - found ") . lookLit)

statePE :: Parser State
statePE = (Reject <$> rejectPE)
      <|> (Accept <$> acceptPE)
      <|> (arrowState False <$> normalArrowPE <*> newStatePE)
      <|> (arrowState True <$> initialArrowPE <*> newStatePE)
      <|> (makeState False <$> normalPE <*> trPE)
      <|> (makeState True <$> initialPE <*> trPE)
      <|> notCharP '}' *> Parser (Just . makeError "expected state keyword - found " . lookLit)
    where
        rejectPE = stringP "reject" *> wsE
                *> stringPE "state" *> wsE
                *> notNullE "expected state name" wordPE <* ws <* charPE ';'
        acceptPE = stringP "accept" *> wsE
                *> stringPE "state" *> wsE
                *> notNullE "expected state name" wordPE <* ws <* charPE ';'
        normalPE = stringP "state" *> wsE
                *> notNullE "expected state name" wordPE <* ws
        initialPE = stringP "initial" *> wsE
                *> normalPE
        trPE = charPE '{' *> commPE *>
            (notNullE "state can't have 0 transitions" . many) (transitionPE <*  commPE)
            <* ws <* charPE '}'
        normalArrowPE = stringP "state" *> wsE
            *> notNullE "expected state name" wordPE <* ws
        initialArrowPE = stringP "initial" *> wsE *> normalArrowPE
        newStatePE = stringP "->" *> ws *> notNullE "expected state name" wordPE <* ws <* charPE ';'

        makeState :: Bool -> String -> [Transition] -> State
        makeState initial name transitions = State name transitions initial
        arrowState :: Bool -> String -> String -> State 
        arrowState initial name newState = State name [Transition '_' '_' N newState] initial

machinePE :: Parser Automaton
machinePE =
    Machine
      <$> (stringP "automaton" *> wsE *> notNullE "expected automaton name" wordPE <* ws)
      <*> (charP '(' *> sepBy comma pair <* ws <* charPE ')' <* ws)
      <*> (charPE '{' *> commPE *>
          (notNullE "machine can't have 0 states" . many) (ws *> statePE <* ws <* commPE)
           <* charPE '}')
    where
        comma = ws *> charP ',' <* ws
        pair :: Parser (String, String)
        pair = notCharP ')' *> ((,) <$> (notNullE "expected component type" wordPE)
                                    <*> (wsE *> notNullE "expected component name" wordPE))

-- Macro Parsers
complementPE :: Parser MacroKeyword
complementPE = Complement
    <$> (stringP "complement" *> ws *> charPE '(' *> ws
         *> notNullE "expected machine type" wordPE <* ws <* charPE ')')

intersectPE :: Parser MacroKeyword
intersectPE = Intersect
    <$> (stringP "intersect" *> ws *> charPE '(' *> ws *>
         sepBy (ws *> charP ',' <* ws) (notNullE "expected machine type" wordPE)
         <* ws <* charPE ')')

reunionPE :: Parser MacroKeyword
reunionPE = Reunion
    <$> (stringP "reunion" *> ws *> charPE '(' *> ws *>
         sepBy (ws *> charP ',' <* ws) (notNullE "expected machine type" wordPE)
         <* ws <* charPE ')')

chainPE :: Parser MacroKeyword
chainPE = Chain
    <$> (stringP "chain" *> ws *> charPE '(' *> ws *>
         sepBy (ws *> charP ',' <* ws) (notNullE "expected machine type" wordPE)
         <* ws <* charPE ')')

repeatPE :: Parser MacroKeyword
repeatPE = Repeat
    <$> (stringP "repeat" *> ws *> charPE '(' *> ws *> numberPE <* ws)
    <*> (charPE ',' *> ws *> (notNullE "expected machine type" wordPE) <* ws <* charPE ')')

moveMPE :: Parser MacroKeyword
moveMPE = Move
    <$> (stringP "move" *> ws *> charPE '(' *> ws *> movePE <* ws)
    <*> (charPE ',' *> ws *> numberPE <* ws <* charPE ')')

overridePE :: Parser MacroKeyword
overridePE = Override
    <$> (stringP "override" *> ws *> charPE '(' *> ws *> movePE <* ws)
    <*> (charPE ',' *> ws *> numberPE <* ws <* charPE ',' <* ws)
    <*> (charPE '\'' *> symbolPE <* charPE '\''  <* ws <* charPE ')')

placePE :: Parser MacroKeyword
placePE = Place
    <$> (stringP "place" *> ws *> charPE '(' *> ws *>
         charPE '"' *> tapePE <* charPE '"' <* ws <* charPE ')')

shiftPE :: Parser MacroKeyword
shiftPE = Shift
    <$> (stringP "shift" *> ws *> charPE '(' *> ws *> movePE <* ws)
    <*> (charPE ',' *> ws *> numberPE <* ws <* charPE ')')

macroPE :: Parser Automaton
macroPE = Macro
    <$> (stringP "automaton" *> wsE *> notNullE "expected automaton name" wordPE
        <* ws <* charP '=' <* ws)
    <*> (complementPE <|> intersectPE <|> reunionPE <|> chainPE <|> repeatPE <|> moveMPE <|> overridePE <|> placePE <|> shiftPE)
        <* ws <* charPE ';'

automataPE :: Parser Automaton
automataPE = macroPE <|> machinePE

programPE :: Parser Program
programPE =
    Program <$> many
       (commPE *> automataPE <* commPE <|> notCharP ' ' *> Parser (Just . makeError "unexpected keyword - " . lookLit))
