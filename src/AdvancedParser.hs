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

look :: String -> (String, String)
look [] = ("", "no more input")
look (y:ys) = (ys, "found '" ++ [y] ++ "'")

makeError :: String -> (String, String) -> Either (String, String) b
makeError err (leftover, cause) = Left (leftover, err ++ " - " ++ cause)

-- Parser Combinators
charP :: Char -> Parser Char
charP x = Parser f
    where
        f [] = Nothing
        f (y:ys)
            |y == x = Just(Right (ys, x))
            |otherwise = Nothing
-- runParser (charP 'a') "abc"    ->   Just (Right ("bc",'a'))
-- runParser (charP 'a') "babc"   ->   Nothing

-- If the parser doesn't succed, it returns an error
charPE :: Char -> Parser Char
charPE x = charP x <|> (Parser $ Just . makeError ("Error: Expected'" ++ [x] ++ "'") . look)
-- runParser (charPE 'a') "abc"     ->     Just (Right ("bc",'a'))
-- runParser (charPE 'a') "babc"    ->    
-- Just (Left ("abc","Error: Expected character 'a' but found 'b'"))

moveP :: Parser Move
moveP = (\_ -> L) <$> charP 'L' 
    <|> (\_ -> R) <$> charP 'R'  
    <|> (\_ -> N) <$> charP 'N'

movePE :: Parser Move
movePE = moveP <|> (Parser $ Just . makeError "Error: Expected move symbol" . look)
