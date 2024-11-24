module AdvancedParser where

import Control.Applicative
import AmethystSyntax

newtype Error = Error {getErr :: String}
newtype Parser a = Parser {runParser :: String -> Maybe (String, Either Error a)}

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
look (y:ys) = (ys, Error $ "found '" ++ [y] ++ "'")

lookLit :: String -> (String, Error)
lookLit [] = ("", Error "")
lookLit (y:ys) = if and $ map ($y) [not . (`elem` "(){};= \n"), (`elem` allowedNameSymbols ++ allowedTapeSymbols)]
    then let (fin, Error rest) = lookLit ys
         in (fin, Error $ y:rest)
    else (y:ys, Error "")

makeError :: String -> (String, Error) -> (String, Either Error b)
makeError err (leftover, Error cause) = (leftover, Left $ Error $ err ++ " - " ++ cause)

charPE :: Char -> Parser Char
charPE x = charP x <|> (Parser $ Just . makeError ("Error: Expected '" ++ [x] ++ "'") . look )

movePE :: Parser Move
movePE = moveP <|> (Parser $ Just . makeError "Error: Expected move symbol" . look)

symbolPE :: Parser Char
symbolPE = symbolP <|> (Parser $ Just . makeError "Error: Expected tape symbol" . look)

wordPE :: Parser String
wordPE = (condition (all (`elem` allowedNameSymbols)) literalP) <|> (Parser $ Just . makeError "Error: Forbidden symbol in word: " . lookLit)

tapePE :: Parser String
tapePE = (condition (all (`elem` allowedTapeSymbols)) literalP) <|> (Parser $ Just . makeError "Error: Forbidden tape in sequence: " . lookLit)

notNullE :: Parser [a] -> Parser [a]
notNullE (Parser p) = Parser $ \input -> do
                (input', res) <- p input
                case res of 
                    Left err -> Just $ (input', Left err)
                    Right el -> if null el 
                        then Just (input', Left $ Error "Error: Null sequence")
                        else Just (input', Right el)

transitionPE :: Parser Transition
transitionPE = Transition
    <$> (ws *> symbolPE <* ws <* charPE '/')
    <*> (ws *> symbolPE <* ws <* charPE ',')
    <*> (ws *> movePE <* ws <* charPE '-' <* charPE '>')
    <*> (ws *> notNullE wordPE <* ws <* charPE ';')
