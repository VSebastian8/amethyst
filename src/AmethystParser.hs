module AmethystParser where

import AdvancedParser
import SyntaxExamples

main :: IO ()
main = do
    let Just (_, Right r) = runParser programPE (Leftover program1 0 0)
    print $ show r
