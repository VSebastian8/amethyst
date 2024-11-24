module AdvancedParser where

import Control.Applicative
import AmethystSyntax

newtype Error = Error {getErr :: String}
newtype Parser a = Parser {runParser :: String -> Maybe (String, Either Error a)}

instance Show Error where
    show (Error e) = "Error: " ++ e 

-- Parser instances
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
            (input', res) <- p input
            case res of
                Left err -> Just (input', Left err)
                Right el -> Just (input', Right $ f el)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, Right x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
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
    (Parser p1) <|> (Parser p2) = Parser $ \input -> do
            p1 input <|> p2 input 

-- Basic parser combinators
charP :: Char -> Parser Char
charP x = Parser f
    where
        f [] = Nothing
        f (y:ys)
            |y == x = Just (ys, Right x)
            |otherwise = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> 
    let (token, rest) = span f input
    in Just (rest, Right token)

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
numberP = read <$> spanP (`elem` "0123456789")

literalP :: Parser String
literalP = spanP (\c ->
    and $ map ($c) [not . (`elem` "(){};= \n"), (`elem` allowedNameSymbols ++ allowedTapeSymbols)])

wordP :: Parser String
wordP = spanP (`elem` allowedNameSymbols)

tapeP :: Parser String
tapeP = spanP (`elem` allowedTapeSymbols)

symbolP :: Parser Char
symbolP = foldr1 (<|>) $ map charP allowedTapeSymbols

moveP :: Parser Move
moveP = (\_ -> L) <$> charP 'L' 
    <|> (\_ -> R) <$> charP 'R'  
    <|> (\_ -> N) <$> charP 'N'

-- Advanced parser combinator
-- If the PE parsers don't succed, they return an error
look :: String -> (String, Error)
look [] = ("", Error "no more input")
look (y:ys) = (ys, Error $ "'" ++ [y] ++ "'")

lookLit :: String -> (String, Error)
lookLit [] = ("", Error "")
lookLit (y:ys) = if and $ map ($y) [not . (`elem` "(){};= \n"), (`elem` allowedNameSymbols ++ allowedTapeSymbols)]
    then let (fin, Error rest) = lookLit ys
         in (fin, Error $ y:rest)
    else (y:ys, Error "")

makeError :: String -> (String, Error) -> (String, Either Error b)
makeError err (leftover, Error cause) = (leftover, Left $ Error $ err ++ cause)

charPE :: Char -> Parser Char
charPE x = charP x 
    <|> (Parser $ Just . makeError ("expected '" ++ [x] ++ "' - found ") . look )

movePE :: Parser Move
movePE = moveP 
    <|> (Parser $ Just . makeError "expected move symbol - found " . look)

notNullE :: String -> Parser [a] -> Parser [a]
notNullE msg (Parser p) = Parser $ \input -> do
                (input', res) <- p input
                case res of 
                    Left err -> Just (input', Left err)
                    Right el -> if null el 
                        then Just (input', Left $ Error msg)
                        else Just (input', Right el)

-- Check that the first character of the input isn't x, it doesn't consume x
notCharP :: Char -> Parser Char
notCharP x = Parser lookC 
        where 
            lookC [] = Nothing
            lookC input@(y:_) =
                if y == x 
                    then Nothing
                    else Just (input, Right ' ')

symbolPE :: Parser Char
symbolPE = symbolP 
    <|> (Parser $ Just . makeError "expected tape symbol - found " . look)

wordPE :: Parser String
wordPE = (condition (all (`elem` allowedNameSymbols)) literalP) 
    <|> (Parser $ Just . makeError "forbidden symbol in word - " . lookLit)

tapePE :: Parser String
tapePE = (condition (all (`elem` allowedTapeSymbols)) literalP) 
    <|> (Parser $ Just . makeError "forbidden tape in sequence - " . lookLit)

transitionPE :: Parser Transition
transitionPE = Transition
    <$> (ws *> notCharP '}' *> symbolPE <* ws <* charPE '/')
    <*> (ws *> symbolPE <* ws <* charPE ',')
    <*> (ws *> movePE <* ws <* charPE '-' <* charPE '>')
    <*> (ws *> notNullE "expected new state" wordPE <* ws <* charPE ';')

wsE :: Parser String
wsE = some (charP ' ') 
    <|> (Parser $ Just . makeError "expected space - found " . look)

stringPE :: String -> Parser String
stringPE str = stringP str 
    <|> (Parser $ Just . makeError ("expected \"" ++ str ++ "\" - found ") . lookLit)

statePE :: Parser State
statePE = (Reject <$> rejectPE)
      <|> (Accept <$> acceptPE)
      <|> ((makeState True) <$> initialPE <*> trPE)
      <|> ((makeState False) <$> normalPE <*> trPE)
      <|> notCharP '}' *> (Parser $ Just . makeError "expected state keyword - found " . lookLit)
    where 
        rejectPE = stringP "reject" *> wsE
                *> stringPE "state" *> wsE
                *> notNullE "expected state name" wordPE <* ws <* charPE ';'
        acceptPE = stringP "accept" *> wsE
                *> stringPE "state" *> wsE
                *> notNullE "expected state name" wordPE <* ws <* charPE ';'
        initialPE = stringP "initial" *> wsE
                *> stringPE "state" *> wsE
                *> notNullE "expected state name" wordPE <* ws 
        normalPE = stringP "state" *> wsE
                *> notNullE "expected state name" wordPE <* ws
        trPE = charPE '{' *> 
            (notNullE "state can't have 0 transitions" . many) transitionPE
            <* ws <* charPE '}'
        makeState :: Bool -> String -> [Transition] -> State
        makeState initial name transitions = State name transitions initial

machinePE :: Parser Automata
machinePE = Machine
      <$> (stringP "automata" *> wsE *> notNullE "expected machine name" wordPE <* ws)
      <*> (charPE '(' *> sepBy comma pair <* ws <* charPE ')' <* ws)
      <*> (charPE '{' *> 
        (notNullE "machine can't have 0 states" . many) (ws *> statePE <* ws) 
        <* charPE '}')
    where
        comma = ws *> charP ',' <* ws
        pair :: Parser (String, String)
        pair = notCharP ')' *> ((\s1 s2 -> (s1, s2)) <$> notNullE "expected component type" wordPE <*> (wsE *> notNullE "expected component name" wordPE))
