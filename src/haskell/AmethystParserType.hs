{-# LANGUAGE InstanceSigs #-}
module AmethystParserType where
import AmethystSyntax
import Control.Applicative
import Text.XHtml (input)

newtype Parser a = Parser {run :: (String, Info) -> Maybe ((String, Info), Either String a)}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p
        = Parser $
        \input -> do
            (input', res) <- run p input
            Just (
                input',
                case res of
                    Left err -> Left err
                    Right x -> Right $ f x
                )

instance Applicative Parser where
    pure :: a -> Parser a
    pure x
        = Parser $
        \input -> Just (input, Right x)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p1 <*> p2
        = Parser $
        \input -> do
            (input', res) <- run p1 input
            case res of
                Left err -> Just(input', Left err)
                Right f -> do
                    (input'', res') <- run p2 input'
                    Just (
                        input'',
                        case res' of
                            Left err -> Left err
                            Right x -> Right $ f x
                        )

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2
        = Parser $
        \input -> do
            run p1 input <|> run p2 input

instance Monad Parser where
    return :: a -> Parser a
    return = pure
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f
        = Parser $
        \input -> do
            (input', res) <- run p input
            case res of
                Left err -> Just (input', Left err)
                Right x -> run (f x) input'

(<+>) :: Parser a -> (String -> String) -> Parser a
p <+> f
    = Parser $
    \input -> do
        (input', res) <- run p input
        Just (
            input',
            case res of
                Left err -> Left $ err ++ f (fst input)
                Right x -> Right x
            )
infixl 0 <+>
