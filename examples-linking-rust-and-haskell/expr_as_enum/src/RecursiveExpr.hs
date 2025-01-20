{-# LANGUAGE ForeignFunctionInterface #-}

module RecursiveExpr where

import Foreign
    ( Ptr,
      new,
      castPtr,
      Storable(peek, sizeOf, alignment, peekElemOff, poke, pokeElemOff) )
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

-- Recursive data type
data Expr = Zero | Number Int | Add Expr Expr
    deriving Show

-- Recursive data type we'll send to C (with Ptr)
data ExprC = ZeroC
          | NumberC Int
          | AddC (Ptr ExprC) (Ptr ExprC)
          deriving Show

-- Implementing the Storable instance for ExprC
instance Storable ExprC where
    sizeOf _ = 12  -- 4 bytes for tag + 2 pointers
    alignment _ = alignment (undefined :: Ptr ExprC)

    peek ptr = do
        tag <- peek (castPtr ptr :: Ptr Int)
        case tag of
            0 -> return ZeroC
            1 -> do
                num <- peekElemOff (castPtr ptr :: Ptr Int) 1
                return $ NumberC num
            2 -> do
                left <- peekElemOff (castPtr ptr :: Ptr (Ptr ExprC)) 1
                right <- peekElemOff (castPtr ptr :: Ptr (Ptr ExprC)) 2
                return $ AddC left right
            _ -> undefined

    poke ptr ZeroC = poke (castPtr ptr :: Ptr Int) 0
    poke ptr (NumberC n) = do
        poke (castPtr ptr :: Ptr Int) 1
        pokeElemOff (castPtr ptr :: Ptr Int) 1 n
    poke ptr (AddC l r) = do
        poke (castPtr ptr :: Ptr Int) 2
        pokeElemOff (castPtr ptr :: Ptr (Ptr ExprC)) 1 l
        pokeElemOff (castPtr ptr :: Ptr (Ptr ExprC)) 2 r

-- Conversion between Expr and ExprC
encryptExpr :: Expr -> ExprC
decryptExpr :: ExprC -> Expr

encryptExpr Zero = ZeroC
encryptExpr (Number n) = NumberC n
encryptExpr (Add e1 e2) = AddC (unsafePerformIO $ new $ encryptExpr e1) (unsafePerformIO $ new $ encryptExpr e2)

decryptExpr ZeroC = Zero
decryptExpr (NumberC n) = Number n
decryptExpr (AddC e1 e2) = Add (decryptExpr (unsafePerformIO $ peek e1)) (decryptExpr (unsafePerformIO $ peek e2))

-- Exported function to create an ExprC
foreign export ccall createExpr :: Int -> Int -> IO (Ptr ExprC)
createExpr x y = do
    let expr = encryptExpr (Add (Number x) (Number y))
    new expr

-- Exported function to create a more complex ExprC
foreign export ccall createComplexExpr :: IO (Ptr ExprC)
createComplexExpr = do
    let expr = encryptExpr (Add (Number 12) (Add Zero (Number 25)))
    new expr

-- Evaluate an Expr recursively
evalExpr :: Expr -> Int
evalExpr Zero = 0
evalExpr (Number n) = n
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2

-- Api function - calls evalExpr
foreign export ccall evaluateExpr :: Ptr ExprC -> IO Int

evaluateExpr :: Ptr ExprC -> IO Int
evaluateExpr expr = do
    exp <- peek expr
    return $ evalExpr $ decryptExpr exp

-- Api function to figure out the type of expression
foreign export ccall expressionType :: Ptr ExprC -> IO Int
expressionType expr = do
    exp <- peek expr
    case exp of
        ZeroC -> return 0
        NumberC _ -> return 1
        AddC _ _ -> return 2

-- Api function to get the number from the Number constructor
foreign export ccall getNumber :: Ptr ExprC -> IO Int
getNumber exprPtr = do
    NumberC number <- peek exprPtr
    return number

-- Api function for the first term of Add constructor
foreign export ccall getFirstAdd :: Ptr ExprC -> IO (Ptr ExprC)
getFirstAdd exprPtr = do
    AddC first _ <- peek exprPtr
    return first

-- Api function for the second term of Add constructor
foreign export ccall getSecondAdd :: Ptr ExprC -> IO (Ptr ExprC)
getSecondAdd exprPtr = do
    AddC _ second <- peek exprPtr
    return second
