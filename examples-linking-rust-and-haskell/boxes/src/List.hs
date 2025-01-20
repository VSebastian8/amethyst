{-# LANGUAGE ForeignFunctionInterface #-}

module List where

import Foreign
import Foreign.C
import Control.Monad (zipWithM_) 

newtype Box = Box CInt deriving (Show)

data MyList = MyList Int [Box] deriving (Show)

instance Storable Box where
    sizeOf _ = sizeOf (undefined :: CInt)
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        value <- peek (castPtr ptr :: Ptr CInt)
        return $ Box value

    poke ptr (Box value) = poke (castPtr ptr :: Ptr CInt) value

instance Storable MyList where
    sizeOf _ = sizeOf (undefined :: Int) + sizeOf (undefined :: Ptr Box)
    alignment _ = alignment (undefined :: Ptr Box)

    peek ptr = do
        len <- peek (castPtr ptr :: Ptr Int)
        elemsPtr <- peek (ptr `plusPtr` sizeOf (undefined :: Int) :: Ptr (Ptr Box))
        elems <- mapM (peekElemOff elemsPtr) [0 .. len - 1] -- Fixed
        return $ MyList len elems

    poke ptr (MyList len elems) = do
        let elemSize = length elems * sizeOf (undefined :: Box)
        elemsPtr <- mallocBytes elemSize
        zipWithM_ (pokeElemOff elemsPtr) [0 ..] elems
        poke (castPtr ptr :: Ptr Int) len
        poke (ptr `plusPtr` sizeOf (undefined :: Int) :: Ptr (Ptr Box)) elemsPtr

foreign export ccall generateList :: IO (Ptr MyList)

generateList :: IO (Ptr MyList)
generateList = new $ MyList 17 (map Box [4..20])
