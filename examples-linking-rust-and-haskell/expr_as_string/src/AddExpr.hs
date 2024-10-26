{-# LANGUAGE ForeignFunctionInterface #-}

module AddExpr where

import Foreign.C.Types
import Foreign.C.String

-- Define an algebraic data type
data Expr = Zero | Number CInt | Add Expr Expr

-- Exported function to create an Expr and return its string representation
foreign export ccall createExpr :: CInt -> CInt -> IO CString

createExpr :: CInt -> CInt -> IO CString
createExpr x y = newCString (show (Add (Number x) (Number y)))

instance Show Expr where
    show Zero = "Zero"
    show (Number n) = show n
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"