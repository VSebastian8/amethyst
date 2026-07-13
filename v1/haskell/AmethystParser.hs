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

chrP :: Parser Char
chrP = Parser f
    where
        f ("", _) = Nothing
        f left@(x:_, _) = Just (advance left, Right x)

lookP :: Parser Char
lookP = Parser f
    where
        f ("", _) = Nothing
        f left@(x:_, _) = Just(left, Right x)

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

condP :: Parser a -> (a -> Bool) -> Parser a
condP p f = do
    res <- p
    if f res
        then return res
        else empty

condCharP :: (Char -> Bool) -> Parser Char
condCharP = condP chrP

lit :: String -> String
lit "" = "no more input"
lit s = takeWhile (`notElem` breakSymbols) s

litP :: Parser String
litP = some (condCharP (`notElem` breakSymbols))
    <|> errorP "expected identificator found - " <+> look

firstWrong :: (Char -> Bool) -> String -> String
firstWrong _ "" = ""
firstWrong f (x:xs)
    | f x = firstWrong f xs
    | otherwise = [x]

wordPE :: Parser String
wordPE = condP validSym (\(x:xs) -> x `notElem` ['0'..'9'])
         <|> errorP "identificator " <+> lit <-> " cannot start with number " <+> look
    where
    validSym = condP litP (all (`elem` nameSymbols))
        <|> errorP "found illegal symbol " <+> firstWrong (`elem` nameSymbols) <-> " in literal "  <+> lit

commPE :: Parser String
commPE = lineComm <|> blockComm
    where
        lineComm = charP '-' *> charP '-' *> many (condCharP (/= '\n'))
        blockComm = charP '{' *> charP '-' *> blockContent <* blockEnd
        blockContent = many ((charP '-' <* condP lookP (/= '}')) <|> condCharP (/= '-'))
        blockEnd =  (charP '-' <* charP '}') <|> errorP "unclosed comment"

ws1 :: Parser String
ws1 = many $ condCharP (`elem` " \t")

ws2 :: Parser [Char]
ws2 = some $ condCharP (`elem` " \t")



