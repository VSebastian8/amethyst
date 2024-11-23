module AdvancedParser where

import Control.Applicative
import AmethystSyntax


newtype Parser a = Parser {runParser :: String -> Maybe (Either (String, String) (String, a))}

-- Parser instances
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
            res <- p input
            case res of
                Left (input', err) -> Just $ Left (input', err)
                Right (input', el) -> Just $ Right (input', f el)

instance Applicative Parser where
    pure x = Parser $ \input -> Just $ Right (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
            res <- p1 input
            case res of
                Left (input', err) -> Just $ Left (input', err)
                Right (input', f) -> do
                    res' <- p2 input'
                    case res' of
                        Left (input'', err) -> Just $ Left (input'', err)
                        Right (input'', el) -> Just $ Right (input'', f el)


instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> do
            p1 input <|> p2 input 
-- runParser (charP 'a' <|> charPE 'b') "bc"  ->  Just (Right ("c",'b'))

-- Basic parser combinators
charP :: Char -> Parser Char
charP x = Parser f
    where
        f [] = Nothing
        f (y:ys)
            |y == x = Just $ Right (ys, x)
            |otherwise = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> 
    let (token, rest) = span f input
    in Just $ Right (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
                Right (input', el) <- p input
                if null el 
                    then Nothing
                    else Just $ Right (input', el)

condition :: ([a] -> Bool) -> Parser [a] -> Parser [a]
condition f (Parser p) = Parser $ \input -> do
                Right (input', el) <- p input
                if f el 
                    then Just $ Right (input', el)
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
look :: String -> (String, String)
look [] = ("", "no more input")
look (y:ys) = (ys, "found '" ++ [y] ++ "'")

lookLit :: String -> (String, String)
lookLit [] = ("", "")
lookLit (y:ys) = if and $ map ($y) [not . (`elem` "(){};= \n"), (`elem` allowedNameSymbols ++ allowedTapeSymbols)]
    then let (fin, rest) = lookLit ys
         in (fin, y:rest)
    else (y:ys, "")

makeError :: String -> (String, String) -> Either (String, String) b
makeError err (leftover, cause) = Left (leftover, err ++ " - " ++ cause)

charPE :: Char -> Parser Char
charPE x = charP x <|> (Parser $ Just . makeError ("Error: Expected'" ++ [x] ++ "'") . look)

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
                Right (input', el) <- p input
                if null el 
                    then Just $ Left (input', "null input")
                    else Just $ Right (input', el)
