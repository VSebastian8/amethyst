{-# LANGUAGE ForeignFunctionInterface #-}

module ParserFFI where

-- FFI
import AdvancedParser
import AmethystSyntax
import Foreign
import Foreign.C.String (CString, newCString, peekCString)

-- Avoid Orphan Instances while still separating the FFI code
newtype ErrorF = ErrorF Error

-- Type sent to Rust
data Result = Prog {getProgram :: Program} | Err {getError :: ErrorF}

instance Storable ErrorF where
  sizeOf _ = sizeOf (undefined :: Ptr ErrorF)
  alignment _ = alignment (undefined :: Ptr Error)
  poke ptr (ErrorF err) = do
    str <- newCString $ getErr err
    poke (castPtr ptr :: Ptr CString) str
  peek ptr = do
    cstr <- peek (castPtr ptr :: Ptr CString)
    str <- peekCString cstr
    return $ (ErrorF . Error) str

instance Storable Result where
  sizeOf _ = sizeOf (undefined :: Ptr Result)
  alignment _ = alignment (undefined :: Ptr Result)
  poke ptr (Prog p) = do
    poke (castPtr ptr :: Ptr Int) 0
  -- pokeElemOff p
  poke ptr (Err errF) = do
    poke (castPtr ptr :: Ptr Int) 1
    pokeElemOff (castPtr ptr :: Ptr ErrorF) 1 errF
  peek ptr = do
    tag <- peek (castPtr ptr :: Ptr Int)
    case tag of
      0 -> return $ (Err . ErrorF . Error) "to do"
      1 -> do
        errF <- peekElemOff (castPtr ptr :: Ptr ErrorF) 1
        return $ Err errF
      _ -> return $ (Err . ErrorF . Error) "Unidentified parser error"

-- Functions exposed through the C api
foreign export ccall parse :: CString -> IO (Ptr Result)

parse :: CString -> IO (Ptr Result)
parse _ = new $ (Err . ErrorF . Error) "hello, I am an error"

foreign export ccall "result_type" resultType :: Ptr Result -> IO Int

resultType :: Ptr Result -> IO Int
resultType resPtr = do
  res <- peek resPtr
  case res of
    Prog _ -> return 0
    Err _ -> return 1

foreign export ccall "return_error" returnError :: Ptr Result -> IO CString

returnError :: Ptr Result -> IO CString
returnError resPtr = do
  (Err (ErrorF e)) <- peek resPtr
  newCString (getErr e)