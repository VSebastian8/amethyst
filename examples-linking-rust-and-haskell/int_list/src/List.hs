{-# LANGUAGE ForeignFunctionInterface #-}

module List where

import Foreign
import Foreign.C

foreign export ccall generateList :: IO (Ptr CInt)

generateList :: IO (Ptr CInt)
generateList = do
  let lst = [1, 2, 3, 4, 5]
  ptr <- mallocArray (length lst)
  pokeArray ptr (map fromIntegral lst)
  return ptr
