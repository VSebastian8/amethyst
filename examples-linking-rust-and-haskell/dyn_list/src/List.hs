{-# LANGUAGE ForeignFunctionInterface #-}

module List where

import Foreign
import Foreign.C ( CInt )

data MyList = MyList { listLength :: Int, listElements :: [CInt] }

instance Storable MyList where
    sizeOf _ = sizeOf (undefined :: Int) + sizeOf (undefined :: Ptr CInt)
    alignment _ = alignment (undefined :: Ptr CInt)

    peek ptr = do
        len <- peek (castPtr ptr :: Ptr Int)
        elemsPtr <- peek (ptr `plusPtr` sizeOf (undefined :: Int) :: Ptr (Ptr CInt))
        elems <- peekArray len elemsPtr
        return $ MyList len elems

    poke ptr (MyList len elems) = do
        let elemsSize = length elems * sizeOf (undefined :: CInt)
        elemsPtr <- mallocBytes elemsSize
        pokeArray elemsPtr elems
        poke (castPtr ptr :: Ptr Int) len
        poke (ptr `plusPtr` sizeOf (undefined :: Int) :: Ptr (Ptr CInt)) elemsPtr

foreign export ccall generateList :: IO (Ptr MyList)

generateList :: IO (Ptr MyList)
generateList = new $ MyList 10 [1..10]
