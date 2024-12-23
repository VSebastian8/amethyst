{-# LANGUAGE ForeignFunctionInterface #-}

module Reverse where

import Foreign.C.String

foreign export ccall reverseString :: CString -> IO CString

make_reverse :: String -> String
make_reverse = foldl (\res s -> s : res) ""

reverseString :: CString -> IO CString
reverseString cstr = do
  str <- peekCString cstr
  newCString (make_reverse str)
