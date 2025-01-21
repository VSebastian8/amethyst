{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParserFFI where

-- FFI
import AdvancedParser
import AmethystSyntax
import SyntaxExamples
import Foreign
import Foreign.C.String (CString, newCString, peekCString)
import Control.Monad (zipWithM_, forM_, forM)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Mem
import Distribution.TestSuite (TestInstance(name))

-- Type sent to Rust
data Result = Prog {getProgram :: !Program} | Err {getError :: !Error}

data TransitionC = TransitionC !Char !Char !(Ptr Move) !CString

data StateC = AcceptC !CString | RejectC !CString | StateC !CString !Bool !Int ![Ptr TransitionC]

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
      2 -> N
      _ -> undefined

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

instance Storable StateC where
  sizeOf _ = sizeOf (undefined :: Int)
           + sizeOf (undefined :: Ptr CString)
           + sizeOf (undefined :: Bool)
           + sizeOf (undefined :: Int)
           + sizeOf (undefined :: Ptr (Ptr TransitionC))
  alignment _ =  alignment (undefined :: Ptr CString)
  poke ptr state = case state of
    AcceptC stateName -> do
      poke (castPtr ptr :: Ptr Int) 0
      poke (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) stateName
    RejectC stateName -> do
      poke (castPtr ptr :: Ptr Int) 1
      poke (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) stateName
    StateC stateName isInitial len transitions -> do
      poke (castPtr ptr :: Ptr Int) 2
      poke (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) stateName
      poke (castPtr ptr `plusPtr`
        ( sizeOf (undefined :: Int)
        + sizeOf (undefined :: Ptr CString) )) isInitial
      poke (castPtr ptr `plusPtr`
        ( sizeOf (undefined :: Int)
        + sizeOf (undefined :: Ptr CString)
        + sizeOf (undefined :: Bool) )) len
      -- List allocation (should be freed)
      transitionArray <- mallocBytes $ length transitions * sizeOf(undefined :: Ptr TransitionC)
      forM_ (zip transitions [0..]) $ \(transition, i) -> do
        pokeElemOff (castPtr transitionArray :: Ptr (Ptr TransitionC)) i transition
        poke (castPtr ptr `plusPtr`
          ( sizeOf (undefined :: Int)
          + sizeOf (undefined :: Ptr CString)
          + sizeOf (undefined :: Bool)
          + sizeOf (undefined :: Int) )) transitionArray
  peek ptr = do
    tag <- peek (castPtr ptr :: Ptr Int)
    case tag of
      0 -> do
        stateName <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
        return $ AcceptC stateName
      1 -> do
        stateName <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
        return $ RejectC stateName
      2 -> do
        stateName <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
        isInitial <- peek (castPtr ptr `plusPtr`
          ( sizeOf (undefined :: Int)
          + sizeOf (undefined :: Ptr CString) ))
        len <- peek (castPtr ptr `plusPtr`
          ( sizeOf (undefined :: Int)
          + sizeOf (undefined :: Ptr CString)
          + sizeOf (undefined :: Bool) ))
        transitionArray <- peek (castPtr ptr `plusPtr`
          ( sizeOf (undefined :: Int)
          + sizeOf (undefined :: Ptr CString)
          + sizeOf (undefined :: Bool)
          + sizeOf (undefined :: Int) ))
        transitions <- forM [0..len] $ \i ->
            peekElemOff (castPtr transitionArray :: Ptr (Ptr TransitionC)) i
        return $ StateC stateName isInitial len transitions
      _ -> undefined

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

-- Conversions before Syntax types to Storable C types
encryptTransition :: Transition -> TransitionC
encryptTransition (Transition readSymbol writeSymbol moveSymbol newState) =
  TransitionC readSymbol writeSymbol (unsafePerformIO $ new moveSymbol) (unsafePerformIO $ newCString newState)

encryptState :: State -> StateC
encryptState (Accept name) = AcceptC (unsafePerformIO $ newCString name)
encryptState (Reject name) = RejectC (unsafePerformIO $ newCString name)
encryptState (State name transitions initial) =
  let stateName = unsafePerformIO $ newCString name
      transitionArray = map (unsafePerformIO . new . encryptTransition) transitions
  in StateC stateName initial (length transitions) transitionArray

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

-- State functions
foreign export ccall "state_type" stateType :: Ptr StateC -> IO Int
stateType :: Ptr StateC -> IO Int
stateType statePtr = do
  state <- peek statePtr
  return $ case state of
    AcceptC _ -> 0
    RejectC _ -> 1
    StateC {} -> 2

foreign export ccall "state_name" stateName :: Ptr StateC -> IO CString
stateName :: Ptr StateC -> IO CString
stateName statePtr = do
  state <- peek statePtr
  return $ case state of
    AcceptC name -> name
    RejectC name -> name
    StateC name _ _ _ -> name

-- Test functions
foreign export ccall "test_transition" testTransition :: Int -> IO (Ptr TransitionC)
testTransition :: Int -> IO (Ptr TransitionC)
testTransition n = new.encryptTransition $
  map (\tr -> let (Just (_, Right transition)) = runParser transitionPE (Leftover tr 0 0) in transition) 
  [transition1, transition2]
  !! (n - 1)

foreign export ccall "test_state" testState :: Int -> IO (Ptr StateC)
testState :: Int -> IO (Ptr StateC)
testState n = new.encryptState $
  map(\st -> let (Just (_, Right state)) = runParser statePE (Leftover st 0 0) in state)
  [state1, state2, state3, state4]
  !! (n - 1)

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
