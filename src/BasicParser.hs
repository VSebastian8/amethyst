module BasicParser where

import Control.Applicative

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

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

ws :: Parser String
ws = spanP (\c -> c == ' ' || c == '\n')

-- At least one white space
ws2 :: Parser String 
ws2 = notNull ws

numberP :: Parser Int
numberP = read <$> spanP (`elem` "0123456789")
