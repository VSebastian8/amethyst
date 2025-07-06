module AmethystParser where

import AmethystParserType
import AmethystSyntax
import Control.Applicative

nextLine :: Info -> Info
nextLine (Info line col) = Info (line + 1) 0

nextCol :: Info -> Info
nextCol (Info line col) = Info line (col + 1)

advance :: (String, Info) -> (String, Info)
advance ("", _) = ("", Info (-1) (-1))
advance (x:xs, info)
    |x == '\n' = (xs, nextLine info)
    |otherwise = (xs, nextCol info)

charP :: Char -> Parser Char 
charP x = Parser f
    where 
        f ("", _) = Nothing
        f left@(y:_, _)
            |y == x = Just (advance left, Right x)
            |otherwise = Nothing

errorP :: String -> Parser a
errorP msg = 
    Parser $
    \input -> Just (input, Left msg)

look :: String -> String 
look "" = " no more input"
look (x:_) = [x]

charPE :: Char -> Parser Char 
charPE x = charP x
    <|> errorP ("expected " ++ [x] ++ " found - ") <+> look

