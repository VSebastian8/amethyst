module AmethystParser where

import BasicParser
import SyntaxExamples

main :: IO ()
main = do
    let Just ("", r) = runParser programP auto2
    print $ show r
