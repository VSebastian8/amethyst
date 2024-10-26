{-# LANGUAGE ForeignFunctionInterface #-}

module HaskellFunction where

import Foreign

-- Define the Shape data type
data Shape = Square Int | Rectangle Int Int
    deriving Show

-- Implementing the Storable instance for Shape
instance Storable Shape where
    sizeOf _ = 12  -- 4 bytes for each Int
    alignment _ = alignment (undefined :: Int)
    peek ptr = do
        tag <- peek (castPtr ptr :: Ptr Int)
        case tag of
            0 -> do
                side <- peekElemOff (castPtr ptr :: Ptr Int) 1
                return $ Square side
            _ -> do
                width <- peekElemOff (castPtr ptr :: Ptr Int) 1
                length <- peekElemOff (castPtr ptr :: Ptr Int) 2
                return $ Rectangle width length
    poke ptr (Square side) = do
        poke (castPtr ptr :: Ptr Int) 0
        pokeElemOff (castPtr ptr :: Ptr Int) 1 side
    poke ptr (Rectangle width length) = do
        poke (castPtr ptr :: Ptr Int) 1
        pokeElemOff (castPtr ptr :: Ptr Int) 1 width
        pokeElemOff (castPtr ptr :: Ptr Int) 2 length

-- Exported function to create a Shape based on input and return a pointer to it
foreign export ccall createShape :: Int -> IO (Ptr Shape)

createShape :: Int -> IO (Ptr Shape)
createShape n = new (chooseShape n)

-- Helper function to choose between Square and Rectangle
chooseShape :: Int -> Shape
chooseShape n
  | n > 10    = Square n
  | otherwise = Rectangle n (2 * n)

-- Function to expose whether it's a square or rectangle
foreign export ccall isSquare :: Ptr Shape -> IO Int

isSquare :: Ptr Shape -> IO Int
isSquare shapePtr = do
    shape <- peek shapePtr
    case shape of
        Square _    -> return 1  -- 1 for Square
        Rectangle _ _ -> return 0  -- 0 for Rectangle

-- Functions to extract details
foreign export ccall getSide :: Ptr Shape -> IO Int
getSide :: Ptr Shape -> IO Int
getSide shapePtr = do
    Square side <- peek shapePtr
    return side

foreign export ccall getWidth :: Ptr Shape -> IO Int
getWidth shapePtr = do
    Rectangle width _ <- peek shapePtr
    return width

foreign export ccall getLength :: Ptr Shape -> IO Int
getLength shapePtr = do
    Rectangle _ length <- peek shapePtr
    return length
