{-# LANGUAGE ForeignFunctionInterface #-}

module Reverse where

import Foreign.C.String

foreign export ccall reverseString :: CString -> IO CString

makeReverse :: String -> String
makeReverse = foldl (flip (:)) ""

reverseString :: CString -> IO CString
reverseString cstr = do
  str <- peekCString cstr
  newCString (makeReverse str)
