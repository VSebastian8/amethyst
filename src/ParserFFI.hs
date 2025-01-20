{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParserFFI where

-- FFI
import AdvancedParser
import AmethystSyntax
import Foreign
import Foreign.C.String (CString, newCString, peekCString)
import Control.Monad (zipWithM_) 
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Mem

-- Type sent to Rust
data Result = Prog {getProgram :: Program} | Err {getError :: Error}

data TransitionC = TransitionC !Char !Char !(Ptr Move) !CString

-- Storable for List (holds the length and then the elements)
instance Storable a => Storable ([] a) where
  sizeOf _ = sizeOf (undefined :: Int) + sizeOf (undefined :: Ptr a)
  alignment _ = alignment (undefined :: Ptr a)
  poke ptr list = do
    let elemSize = length list * sizeOf (undefined :: a)
    elemsPtr <- mallocBytes elemSize
    zipWithM_ (pokeElemOff elemsPtr) [0..] list
    poke (castPtr ptr :: Ptr Int) (length list)
    poke (ptr `plusPtr` sizeOf (undefined :: Int) :: Ptr (Ptr a)) elemsPtr
  peek ptr = do
    len <- peek (castPtr ptr :: Ptr Int)
    elemsPtr <- peek (ptr `plusPtr` sizeOf (undefined :: Int) :: Ptr (Ptr a))
    mapM (peekElemOff elemsPtr) [0..len-1]

-- Storable instances for all syntax data types
instance Storable Move where
  sizeOf _ = sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  poke ptr move = case move of
    L -> poke (castPtr ptr :: Ptr Int) 0
    R -> poke (castPtr ptr :: Ptr Int) 1
    N -> poke (castPtr ptr :: Ptr Int) 2
  peek ptr = do
    tag <- peek (castPtr ptr :: Ptr Int)
    return $ case tag of
      0 -> L
      1 -> R
      _ -> N

instance Storable TransitionC where
  sizeOf _ = 2 * sizeOf (undefined :: Ptr Char) 
               + sizeOf (undefined :: Ptr (Ptr Move))
               + sizeOf (undefined :: Ptr CString) 
  alignment _ = alignment (undefined :: CString)
  poke ptr (TransitionC readSymbol writeSymbol movePtr newState) = do
    poke (castPtr ptr) readSymbol
    poke (castPtr ptr `plusPtr` sizeOf (undefined :: Char)) writeSymbol
    poke (castPtr ptr `plusPtr` (2 * sizeOf (undefined :: Char))) movePtr
    poke (castPtr ptr `plusPtr` (2 * sizeOf (undefined :: Char) + sizeOf (undefined :: Ptr Move))) newState
  peek ptr = do
    readSymbol <- peek (castPtr ptr) 
    writeSymbol <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Char)) 
    movePtr <- peek (castPtr ptr `plusPtr` (2 * sizeOf (undefined :: Char)))
    newState <- peek (castPtr ptr `plusPtr` (2 * sizeOf (undefined :: Char) + sizeOf (undefined :: Ptr Move)))
    return $ TransitionC readSymbol writeSymbol movePtr newState

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
foreign export ccall "move_type" moveType :: Ptr Move -> IO Int
moveType :: Ptr Move -> IO Int
moveType movePtr = do
  move <- peek movePtr
  return $ case move of 
    L -> 0
    R -> 1
    N -> 2

-- Transition functions
foreign export ccall "transition_read_symbol" transitionReadSymbol :: Ptr TransitionC -> IO Char
transitionReadSymbol :: Ptr TransitionC -> IO Char
transitionReadSymbol tranPtr = do
  TransitionC readSymbol _ _ _ <- peek tranPtr
  return readSymbol

foreign export ccall "transition_write_symbol" transitionWriteSymbol :: Ptr TransitionC -> IO Char
transitionWriteSymbol :: Ptr TransitionC -> IO Char
transitionWriteSymbol tranPtr = do
  TransitionC _ writeSymbol _ _ <- peek tranPtr
  return writeSymbol

foreign export ccall "transition_move_symbol" transitionMoveSymbol :: Ptr TransitionC -> IO (Ptr Move)
transitionMoveSymbol :: Ptr TransitionC -> IO (Ptr Move)
transitionMoveSymbol tranPtr = do
  TransitionC _ _ movePtr _ <- peek tranPtr
  return movePtr

foreign export ccall "transition_new_state" transitionNewState :: Ptr TransitionC -> IO CString
transitionNewState :: Ptr TransitionC -> IO CString
transitionNewState tranPtr = do
  (TransitionC _ _ _ newState) <- peek tranPtr
  return newState

foreign export ccall "free_transition" freeTransition :: Ptr TransitionC -> IO ()
freeTransition :: Ptr TransitionC -> IO()
freeTransition tranPtr = do
  TransitionC _ _ movePtr newState <- peek tranPtr
  -- putStrLn $ "Freeing transition at: " ++ show tranPtr ++ " with size " ++ show (sizeOf tranPtr) ++ " and alignment " ++ show (alignment tranPtr)
  free movePtr
  free newState
  free tranPtr  

foreign export ccall "test_transition" testTransition :: Int -> IO (Ptr TransitionC)
testTransition :: Int -> IO (Ptr TransitionC)
testTransition n = new $ TransitionC 'x' 'y' (unsafePerformIO $ new (getMove n)) (unsafePerformIO $  newCString ("qstare" ++ show n))
  where getMove 0 = L
        getMove 1 = R
        getMove _ = N

-- Result functions
foreign export ccall "result_type" resultType :: Ptr Result -> IO Int
resultType :: Ptr Result -> IO Int
resultType resPtr = do
  result <- peek resPtr
  case result of
    Prog _ -> return 0
    Err _ -> return 1

foreign export ccall "return_error" returnError :: Ptr Result -> IO CString
returnError :: Ptr Result -> IO CString
returnError resPtr = do
  (Err (Error e)) <- peek resPtr
  newCString e

foreign export ccall parse :: CString -> IO (Ptr Result)
parse :: CString -> IO (Ptr Result)
parse _ = new $ (Err . Error) "hello, I am an error"

foreign export ccall "clean_up" cleanUp :: IO ()
cleanUp :: IO()
cleanUp = System.Mem.performGC
