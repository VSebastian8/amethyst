module Triple where

foreign export ccall triple_input :: Int -> Int

triple_input :: Int -> Int
triple_input a = a * 3
