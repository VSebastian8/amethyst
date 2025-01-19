{-# LANGUAGE ForeignFunctionInterface #-}

module ParserFFI where

-- FFI
import AdvancedParser
import AmethystSyntax
import Foreign
import Foreign.C.String (CString, newCString, peekCString)

-- Type sent to Rust
data Result = Prog {getProgram :: Program} | Err {getError :: Error}

-- Storable instances for all syntax data types
instance Storable Move where
  sizeOf _ = sizeOf (undefined :: Ptr Move)
  alignment _ = sizeOf (undefined :: Ptr Move)
  poke ptr move = undefined
  peek ptr = undefined 

instance Storable Transition where
  sizeOf _ = sizeOf (undefined :: Ptr Transition)
  alignment _ = sizeOf (undefined :: Ptr Transition)
  poke ptr transition = undefined
  peek ptr = undefined 

instance Storable State where
  sizeOf _ = sizeOf (undefined :: Ptr State)
  alignment _ = sizeOf (undefined :: Ptr State)
  poke ptr state = undefined
  peek ptr = undefined 

instance Storable MacroKeyword where
  sizeOf _ = sizeOf (undefined :: Ptr MacroKeyword)
  alignment _ = sizeOf (undefined :: Ptr MacroKeyword)
  poke ptr macro = undefined
  peek ptr = undefined 

instance Storable Automata where
  sizeOf _ = sizeOf (undefined :: Ptr Automata)
  alignment  _ = alignment  (undefined :: Ptr Automata)
  poke ptr automata = do
    case automata of
      Machine {} -> poke (castPtr ptr :: Ptr Int) 0
      Macro {} -> poke (castPtr ptr :: Ptr Int) 1
    -- pokeElemOff autom
  peek ptr = undefined

instance Storable Program where
  sizeOf _ = sizeOf (undefined :: Ptr Program)
  alignment _ = sizeOf (undefined :: Ptr Program)
  poke ptr program = undefined
  peek ptr = undefined 

instance Storable Error where
  sizeOf _ = sizeOf (undefined :: Ptr Error)
  alignment _ = alignment (undefined :: Ptr Error)
  poke ptr (Error err) = do
    str <- newCString err
    poke (castPtr ptr :: Ptr CString) str
  peek ptr = do
    cstr <- peek (castPtr ptr :: Ptr CString)
    str <- peekCString cstr
    return $ Error str

instance Storable Result where
  sizeOf _ = sizeOf (undefined :: Ptr Result)
  alignment _ = alignment (undefined :: Ptr Result)
  poke ptr (Prog p) = do
    poke (castPtr ptr :: Ptr Int) 0
    pokeElemOff (castPtr ptr :: Ptr Program) 1 p
  poke ptr (Err errF) = do
    poke (castPtr ptr :: Ptr Int) 1
    pokeElemOff (castPtr ptr :: Ptr Error) 1 errF
  peek ptr = do
    tag <- peek (castPtr ptr :: Ptr Int)
    case tag of
      0 -> return $ (Err . Error) "to do"
      1 -> do
        errF <- peekElemOff (castPtr ptr :: Ptr Error) 1
        return $ Err errF
      _ -> return $ (Err . Error) "Unidentified parser error"

-- Functions exposed through the C api
foreign export ccall parse :: CString -> IO (Ptr Result)

parse :: CString -> IO (Ptr Result)
parse _ = new $ (Err . Error) "hello, I am an error"

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
  (Err (Error e)) <- peek resPtr
  newCString e
