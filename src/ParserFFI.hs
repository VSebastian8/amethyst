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
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import qualified System.Mem
import Distribution.TestSuite (TestInstance(name))
import Language.Haskell.TH (doublePrimL)
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Data.IntMap (size)

-- Types for the C API
-- data Result = Prog {getProgram :: !Program} | Err {getError :: !Error}

data TransitionC = TransitionC !Char !Char !(Ptr Move) !CString

data StateC
  = AcceptC !CString
  | RejectC !CString
  | StateC !CString !Bool !Int ![Ptr TransitionC]

data MacroKeywordC
  = ComplementC !CString
  | IntersectC !Int ![CString]
  | ReunionC !Int ![CString]
  | ChainC !Int ![CString]
  | RepeatC !Int !CString
  | MoveC !(Ptr Move) !Int
  | OverrideC !(Ptr Move) !Int !Char
  | PlaceC !CString
  | ShiftC !(Ptr Move) !Int

data MachineC = MachineC !Int ![(CString, CString)] !Int ![Ptr StateC]

data AutomataC
  = MachC !CString !(Ptr MachineC)
  | MacroC !CString !(Ptr MacroKeywordC)

data ResultC = ProgramC !Int ![Ptr AutomataC] | ErrorC !CString !Int !Int

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
      poke (castPtr ptr `plusPtr`
        ( sizeOf (undefined :: Int)
        + sizeOf (undefined :: Ptr CString)
        + sizeOf (undefined :: Bool)
        + sizeOf (undefined :: Int) )) transitionArray
      forM_ (zip transitions [0..]) $ \(transition, i) -> do
        pokeElemOff (castPtr transitionArray :: Ptr (Ptr TransitionC)) i transition
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
        transitions <- forM [0..len - 1] $ \i ->
            peekElemOff (castPtr transitionArray :: Ptr (Ptr TransitionC)) i
        return $ StateC stateName isInitial len transitions
      _ -> undefined

instance Storable MacroKeywordC where
  sizeOf _ = 2 * sizeOf (undefined :: Int)
               + sizeOf(undefined :: Ptr Move)
               + sizeOf (undefined :: Char)
  alignment _ = alignment (undefined :: Ptr CString)
  poke ptr macro = case macro of
    ComplementC automata -> do
      poke (castPtr ptr :: Ptr Int) 0
      poke (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) automata
    IntersectC len automataList -> do
      poke (castPtr ptr :: Ptr Int) 1
      poke (castPtr ptr `plusPtr` sizeOf(undefined :: Int)) len
      -- List allocation (should be freed)
      automataArray <- mallocBytes $ len * sizeOf(undefined :: Ptr CString)
      poke (castPtr ptr `plusPtr` ( 2 * sizeOf(undefined :: Int) )) automataArray
      forM_ (zip automataList [0..]) $ \(automata, i) -> do
        pokeElemOff (castPtr automataArray :: Ptr CString) i automata
    ReunionC len automataList -> do
      poke (castPtr ptr :: Ptr Int) 2
      poke (castPtr ptr `plusPtr` sizeOf(undefined :: Int)) len
      -- List allocation (should be freed)
      automataArray <- mallocBytes $ len * sizeOf(undefined :: Ptr CString)
      poke (castPtr ptr `plusPtr` ( 2 * sizeOf(undefined :: Int) )) automataArray
      forM_ (zip automataList [0..]) $ \(automata, i) -> do
        pokeElemOff (castPtr automataArray :: Ptr CString) i automata
    ChainC len automataList -> do
      poke (castPtr ptr :: Ptr Int) 3
      poke (castPtr ptr `plusPtr` sizeOf(undefined :: Int)) len
      -- List allocation (should be freed)
      automataArray <- mallocBytes $ len * sizeOf(undefined :: Ptr CString)
      poke (castPtr ptr `plusPtr` ( 2 * sizeOf(undefined :: Int) )) automataArray
      forM_ (zip automataList [0..]) $ \(automata, i) -> do
              pokeElemOff (castPtr automataArray :: Ptr CString) i automata
    RepeatC number automata -> do
      poke (castPtr ptr :: Ptr Int) 4
      poke (castPtr ptr `plusPtr` sizeOf(undefined :: Int)) number
      poke (castPtr ptr `plusPtr` ( 2 * sizeOf(undefined :: Int) )) automata
    MoveC move number -> do
      poke (castPtr ptr :: Ptr Int) 5
      poke (castPtr ptr `plusPtr` sizeOf(undefined :: Int)) move
      poke (castPtr ptr `plusPtr`
          ( sizeOf(undefined :: Int)
          + sizeOf(undefined :: Ptr Move) )) number
    OverrideC move number symbol -> do
      poke (castPtr ptr :: Ptr Int) 6
      poke (castPtr ptr `plusPtr` sizeOf(undefined :: Int)) move
      poke (castPtr ptr `plusPtr`
          ( sizeOf(undefined :: Int)
          + sizeOf(undefined :: Ptr Move) )) number
      poke (castPtr ptr `plusPtr`
          ( 2 * sizeOf(undefined :: Int)
          + sizeOf(undefined :: Ptr Move) )) symbol
    PlaceC text -> do
      poke (castPtr ptr :: Ptr Int) 7
      poke (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) text
    ShiftC move number -> do
      poke (castPtr ptr :: Ptr Int) 8
      poke (castPtr ptr `plusPtr` sizeOf(undefined :: Int)) move
      poke (castPtr ptr `plusPtr`
          ( sizeOf(undefined :: Int)
          + sizeOf(undefined :: Ptr Move) )) number
  peek ptr = do
    tag <- peek (castPtr ptr :: Ptr Int)
    case tag of
      0 -> do
        automata <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
        return $ ComplementC automata
      1 -> do
        len <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
        automataArray <-  peek (castPtr ptr `plusPtr` ( 2 * sizeOf(undefined :: Int) ))
        automataList <- forM [0..len - 1] $ \i ->
            peekElemOff (castPtr automataArray :: Ptr CString) i
        return $ IntersectC len automataList
      2 -> do
        len <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
        automataArray <-  peek (castPtr ptr `plusPtr` ( 2 * sizeOf(undefined :: Int) ))
        automataList <- forM [0..len - 1] $ \i ->
            peekElemOff (castPtr automataArray :: Ptr CString) i
        return $ ReunionC len automataList
      3 -> do
        len <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
        automataArray <- peek (castPtr ptr `plusPtr` ( 2 * sizeOf(undefined :: Int) ))
        automataList <- forM [0..len - 1] $ \i ->
            peekElemOff (castPtr automataArray :: Ptr CString) i
        return $ ChainC len automataList
      4 -> do
        number <- peek (castPtr ptr `plusPtr` sizeOf(undefined :: Int))
        automata <- peek (castPtr ptr `plusPtr` ( 2 * sizeOf(undefined :: Int) ))
        return $ RepeatC number automata
      5 -> do
        move <- peek (castPtr ptr `plusPtr` sizeOf(undefined :: Int))
        number <- peek (castPtr ptr `plusPtr`
            ( sizeOf(undefined :: Int)
            + sizeOf(undefined :: Ptr Move) ))
        return $ MoveC move number
      6 -> do
        move <- peek (castPtr ptr `plusPtr` sizeOf(undefined :: Int))
        number <- peek (castPtr ptr `plusPtr`
            ( sizeOf(undefined :: Int)
            + sizeOf(undefined :: Ptr Move) ))
        symbol <- peek (castPtr ptr `plusPtr`
            ( 2 * sizeOf(undefined :: Int)
            + sizeOf(undefined :: Ptr Move) ))
        return $ OverrideC move number symbol
      7 -> do
        text <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
        return $ PlaceC text
      8 -> do
        move <- peek (castPtr ptr `plusPtr` sizeOf(undefined :: Int))
        number <- peek (castPtr ptr `plusPtr`
          ( sizeOf(undefined :: Int)
          + sizeOf(undefined :: Ptr Move) ))
        return $ ShiftC move number
      _ -> undefined

instance Storable MachineC where
  sizeOf _ = 2 * sizeOf(undefined :: Int)
               + sizeOf(undefined :: Ptr (Ptr CString))
               + sizeOf(undefined :: Ptr (Ptr StateC))
  alignment  _ = alignment  (undefined :: Ptr (Ptr CString))
  poke ptr (MachineC len1 components len2 states) = do
      -- Components list
      poke (castPtr ptr :: Ptr Int) len1
      componentsArray <- mallocBytes $ len1 * 2 * sizeOf (undefined :: Ptr CString)
      poke (castPtr ptr `plusPtr` sizeOf(undefined :: Int)) componentsArray
      forM_ (zip components [0,2..]) $ \((compType, compName), i) -> do
        pokeElemOff (castPtr componentsArray :: Ptr CString) i compType
        pokeElemOff (castPtr componentsArray :: Ptr CString) (i + 1) compName
      -- States list
      poke (castPtr ptr `plusPtr`
          ( sizeOf(undefined :: Int)
          + sizeOf(undefined :: Ptr (Ptr CString)) )) len2
      statesArray <- mallocBytes $ len2 * sizeOf (undefined :: Ptr StateC)
      poke (castPtr ptr `plusPtr`
          ( 2 * sizeOf(undefined :: Int)
          + sizeOf(undefined :: Ptr (Ptr CString)) )) statesArray
      forM_ (zip states [0..]) $ \(state, i) -> do
        pokeElemOff (castPtr statesArray :: Ptr (Ptr StateC)) i state
  peek ptr = do
      -- Components list
      len1 <- peek (castPtr ptr :: Ptr Int)
      componentsArray <- peek (castPtr ptr `plusPtr` sizeOf(undefined :: Int))
      components <- forM [0..(len1 - 1)] $ \i -> do
        compType <- peekElemOff (castPtr componentsArray :: Ptr CString) (i * 2)
        compName <- peekElemOff (castPtr componentsArray :: Ptr CString) (i * 2 + 1)
        return (compType, compName)
      -- States list
      len2 <- peek (castPtr ptr `plusPtr`
            ( sizeOf(undefined :: Int)
            + sizeOf(undefined :: Ptr (Ptr CString)) ))
      statesArray <- peek (castPtr ptr `plusPtr`
            ( 2 * sizeOf(undefined :: Int)
            + sizeOf(undefined :: Ptr (Ptr CString)) ))
      states <- forM [0..(len2 - 1)] $ \i -> peekElemOff (castPtr statesArray :: Ptr (Ptr StateC)) i
      return $ MachineC len1 components len2 states


instance Storable AutomataC where
  sizeOf _ = sizeOf (undefined :: Ptr CString)
       + 2 * sizeOf (undefined :: Ptr Int)
  alignment  _ = alignment  (undefined :: Ptr CString)
  poke ptr automata =
    case automata of
      MachC name machine -> do
        poke (castPtr ptr :: Ptr Int) 0
        poke (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) name
        poke (castPtr ptr `plusPtr`
            ( sizeOf (undefined :: Int)
            + sizeOf (undefined :: Ptr CString) )) machine
      MacroC name keyword  -> do
        poke (castPtr ptr :: Ptr Int) 1
        poke (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) name
        poke (castPtr ptr `plusPtr`
            ( sizeOf (undefined :: Int)
            + sizeOf (undefined :: Ptr CString) )) keyword
  peek ptr = do
    tag <- peek (castPtr ptr :: Ptr Int)
    name <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
    case tag of
      0 -> do
        machine <- peek (castPtr ptr `plusPtr`
          ( sizeOf (undefined :: Int)
          + sizeOf (undefined :: Ptr CString) ))
        return $ MachC name machine
      1 -> do
        keyword <- peek (castPtr ptr `plusPtr`
          ( sizeOf (undefined :: Int)
          + sizeOf (undefined :: Ptr CString) ))
        return $ MacroC name keyword
      _ -> undefined

instance Storable ResultC where
  sizeOf _ = 3 * sizeOf (undefined :: Ptr Int) + sizeOf (undefined :: Ptr CString)
  alignment _ = alignment (undefined :: Ptr CString)
  poke ptr (ProgramC len automataList) = do
    poke (castPtr ptr :: Ptr Int) 0
    poke (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) len
    automataArray <- mallocBytes $ length automataList * sizeOf (undefined :: Ptr AutomataC)
    poke (castPtr ptr `plusPtr` (2 * sizeOf (undefined :: Int))) automataArray
    forM_ (zip automataList [0..]) $ \(automata, i) -> do
      pokeElemOff (castPtr automataArray :: Ptr (Ptr AutomataC)) i automata
  poke ptr (ErrorC error line column) = do
    poke (castPtr ptr :: Ptr Int) 1
    poke (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) error
    poke (castPtr ptr `plusPtr` 
        ( sizeOf (undefined :: Int) 
        + sizeOf (undefined :: Ptr CString) )) line 
    poke (castPtr ptr `plusPtr` 
        ( 2 * sizeOf (undefined :: Int) 
        + sizeOf (undefined :: Ptr CString) )) column
  peek ptr = do
    tag <- peek (castPtr ptr :: Ptr Int)
    case tag of
      0 -> do
        len <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
        automataArray <- peek (castPtr ptr `plusPtr` (2 * sizeOf (undefined :: Int)))
        automataList <- forM [0..len - 1] $ \i ->
            peekElemOff (castPtr automataArray :: Ptr (Ptr AutomataC)) i
        return $ ProgramC len automataList
      1 -> do
        error <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) 
        line <- peek (castPtr ptr `plusPtr` 
           ( sizeOf (undefined :: Int) 
           + sizeOf (undefined :: Ptr CString) )) 
        column <- peek (castPtr ptr `plusPtr` 
           ( 2 * sizeOf (undefined :: Int) 
           + sizeOf (undefined :: Ptr CString) ))
        return $ ErrorC error line column
      _ -> undefined

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

encryptMacro :: MacroKeyword -> MacroKeywordC
encryptMacro (Complement automata) = ComplementC (unsafePerformIO $ newCString automata)
encryptMacro (Intersect automataList) = IntersectC (length automataList) (map (unsafePerformIO.newCString) automataList)
encryptMacro (Reunion automataList) = ReunionC (length automataList) (map (unsafePerformIO.newCString) automataList)
encryptMacro (Chain automataList) = ChainC (length automataList) (map (unsafePerformIO.newCString) automataList)
encryptMacro (Repeat number automata) = RepeatC number (unsafePerformIO $ newCString automata)
encryptMacro (Move move number) = MoveC (unsafePerformIO $ new move) number
encryptMacro (Override move number symbol) = OverrideC (unsafePerformIO $ new move) number symbol
encryptMacro (Place text) = PlaceC (unsafePerformIO $ newCString  text)
encryptMacro (Shift move number) = ShiftC (unsafePerformIO $ new move) number

encryptAutomata :: Automata -> AutomataC
encryptAutomata (Machine name components states) =
  let machineName = unsafePerformIO $ newCString name
      machineComponents = map (\(s1,s2) -> (unsafePerformIO $ newCString s1,  unsafePerformIO $ newCString s2)) components
      machineStates = map (unsafePerformIO . new . encryptState) states
  in MachC machineName (unsafePerformIO.new $ MachineC (length components) machineComponents (length states) machineStates)
encryptAutomata (Macro name keyword) =
  let macroName = unsafePerformIO $ newCString name
      macroKeyword = unsafePerformIO $ new $ encryptMacro keyword
  in MacroC macroName macroKeyword

runAmethystParser :: String -> ResultC
runAmethystParser code = case runParser programPE (Leftover code 0 0) of
    Nothing -> ErrorC (unsafePerformIO $ newCString "Unidentified parser error") 0 0
    Just (Leftover _ line column, result) ->
      case result of
        Left (Error error) -> ErrorC (unsafePerformIO $ newCString error) line column 
        Right (Program automata) -> ProgramC (length automata) (map (unsafePerformIO.new.encryptAutomata) automata)

-- Function used to parse the input string
foreign export ccall "amethyst_parser" amethystParser :: CString -> IO (Ptr ResultC)
amethystParser codeC = do
  code <- peekCString codeC
  new $ runAmethystParser code

-- Functions used to reconstruct the syntax through the C api
foreign export ccall "move_type" moveType :: Ptr Move -> IO Int
moveType movePtr = do
  move <- peek movePtr
  return $ case move of
    L -> 0
    R -> 1
    N -> 2

-- Transition functions
foreign export ccall "transition_read_symbol" transitionReadSymbol :: Ptr TransitionC -> IO Char
transitionReadSymbol tranPtr = do
  TransitionC readSymbol _ _ _ <- peek tranPtr
  return readSymbol

foreign export ccall "transition_write_symbol" transitionWriteSymbol :: Ptr TransitionC -> IO Char
transitionWriteSymbol tranPtr = do
  TransitionC _ writeSymbol _ _ <- peek tranPtr
  return writeSymbol

foreign export ccall "transition_move_symbol" transitionMoveSymbol :: Ptr TransitionC -> IO (Ptr Move)
transitionMoveSymbol tranPtr = do
  TransitionC _ _ movePtr _ <- peek tranPtr
  return movePtr

foreign export ccall "transition_new_state" transitionNewState :: Ptr TransitionC -> IO CString
transitionNewState tranPtr = do
  (TransitionC _ _ _ newState) <- peek tranPtr
  return newState

foreign export ccall "free_transition" freeTransition :: Ptr TransitionC -> IO ()
freeTransition tranPtr = do
  TransitionC _ _ movePtr newState <- peek tranPtr
  free movePtr
  free newState
  free tranPtr

-- State functions
foreign export ccall "state_type" stateType :: Ptr StateC -> IO Int
stateType statePtr = do
  state <- peek statePtr
  return $ case state of
    AcceptC _ -> 0
    RejectC _ -> 1
    StateC {} -> 2

foreign export ccall "state_name" stateName :: Ptr StateC -> IO CString
stateName statePtr = do
  state <- peek statePtr
  return $ case state of
    AcceptC name -> name
    RejectC name -> name
    StateC name _ _ _ -> name

foreign export ccall "state_is_initial" stateInitial :: Ptr StateC -> IO Bool
stateInitial statePtr = do
  state <- peek statePtr
  return $ case state of
    StateC _ True _ _ -> True
    _ -> False

foreign export ccall "state_tr_len" stateTransitionsLength :: Ptr StateC -> IO Int
stateTransitionsLength statePtr = do
  state <- peek statePtr
  return $ case state of
    StateC _ _ n _ -> n
    _ -> 0

foreign export ccall "state_transitions" stateTransitionsPtr :: Ptr StateC -> IO (Ptr [Ptr TransitionC])
stateTransitionsPtr statePtr =
  peek $ castPtr statePtr `plusPtr`
          ( sizeOf (undefined :: Int)
          + sizeOf (undefined :: Ptr CString)
          + sizeOf (undefined :: Bool)
          + sizeOf (undefined :: Int) )

foreign export ccall "state_transition_i" stateTransitionI :: Ptr (Ptr TransitionC) -> Int -> IO (Ptr TransitionC)
stateTransitionI = peekElemOff

foreign export ccall "free_state" freeState :: Ptr StateC -> IO ()
freeState statePtr = do
  state <- peek statePtr
  case state of
    AcceptC namePtr -> free namePtr
    RejectC namePtr -> free namePtr
    StateC namePtr _ _ transitions -> do
      forM_ transitions freeTransition
      transitionArray <- peek $ castPtr statePtr `plusPtr`
            ( sizeOf (undefined :: Int)
            + sizeOf (undefined :: Ptr CString)
            + sizeOf (undefined :: Bool)
            + sizeOf (undefined :: Int) )
      free transitionArray
      free namePtr
  free statePtr

-- Macro functions
foreign export ccall "macro_type" macroType :: Ptr MacroKeywordC -> IO Int
macroType macroPtr = do
  macro <- peek macroPtr
  return $ case macro of
    ComplementC {} -> 0
    IntersectC {} -> 1
    ReunionC {} -> 2
    ChainC {} -> 3
    RepeatC {} -> 4
    MoveC {} -> 5
    OverrideC {} -> 6
    PlaceC {} -> 7
    ShiftC {} -> 8

foreign export ccall "macro_string" macroString :: Ptr MacroKeywordC -> IO CString
macroString macroPtr = do
  macro <- peek macroPtr
  return $ case macro of
    ComplementC automata -> automata
    RepeatC _ automata -> automata
    PlaceC text -> text
    _ -> undefined

foreign export ccall "macro_number" macroNumber :: Ptr MacroKeywordC -> IO Int
macroNumber macroPtr = do
  macro <- peek macroPtr
  return $ case macro of
    RepeatC n _ -> n
    MoveC _ n -> n
    OverrideC _ n _ -> n
    ShiftC _ n -> n
    _ -> undefined

foreign export ccall "macro_move" macroMove :: Ptr MacroKeywordC -> IO (Ptr Move)
macroMove macroPtr = do
  macro <- peek macroPtr
  return $ case macro of
    MoveC m _ -> m
    OverrideC m _ _ -> m
    ShiftC m _ -> m
    _ -> undefined

foreign export ccall "macro_symbol" macroSymbol :: Ptr MacroKeywordC -> IO Char
macroSymbol macroPtr = do
  macro <- peek macroPtr
  return $ case macro of
    OverrideC _ _ c -> c
    _ -> undefined

foreign export ccall "macro_list_len" macroListLen :: Ptr MacroKeywordC -> IO Int
macroListLen macroPtr = do
  macro <- peek macroPtr
  return $ case macro of
    IntersectC len _ -> len
    ReunionC len _ -> len
    ChainC len _ -> len
    _ -> undefined

foreign export ccall "macro_list" macroList :: Ptr MacroKeywordC -> IO (Ptr [CString])
macroList macroPtr =
  peek $ castPtr macroPtr `plusPtr` (2 * sizeOf(undefined :: Int))

foreign export ccall "macro_list_i" macroListI :: Ptr (Ptr CString) -> Int -> IO (Ptr CString)
macroListI = peekElemOff

freeMacro :: Ptr MacroKeywordC -> IO()
freeMacro macroPtr = do
  macro <- peek macroPtr
  case macro of
    ComplementC automata -> free automata
    IntersectC _ automataList -> do
      forM_ automataList free
      automataArray <- peek (castPtr macroPtr `plusPtr` (2 * sizeOf(undefined :: Int)))
      free automataArray
    ReunionC _ automataList -> do
      forM_ automataList free
      automataArray <- peek (castPtr macroPtr `plusPtr` (2 * sizeOf(undefined :: Int)))
      free automataArray
    ChainC _ automataList -> do
      forM_ automataList free
      automataArray <- peek (castPtr macroPtr `plusPtr` (2 * sizeOf(undefined :: Int)))
      free automataArray
    RepeatC _ automata -> free automata
    MoveC move _ -> free move
    OverrideC move _ _ -> free move
    PlaceC text -> free text
    ShiftC move _ -> free move
  free macroPtr

-- Machine functions
foreign export ccall "machine_components_len" machineComponentsLen :: Ptr MachineC -> IO Int
machineComponentsLen machinePtr = do
  (MachineC len1 _ _ _) <- peek machinePtr
  return len1

foreign export ccall "machine_components" machineComponents :: Ptr MachineC -> IO (Ptr [CString])
machineComponents machinePtr = peek $ castPtr machinePtr `plusPtr` sizeOf(undefined :: Int)

foreign export ccall "machine_components_i_first" machineComponentsFirst :: Ptr (Ptr CString) -> Int -> IO (Ptr CString)
machineComponentsFirst machinePtr i = peekElemOff machinePtr (i * 2)

foreign export ccall "machine_components_i_second" machineComponentsSecond :: Ptr (Ptr CString) -> Int -> IO (Ptr CString)
machineComponentsSecond machinePtr i = peekElemOff machinePtr (i * 2 + 1)

foreign export ccall "machine_states_len" machineStatesLen :: Ptr MachineC -> IO Int
machineStatesLen machinePtr = do
  (MachineC _ _ len2 _) <- peek machinePtr
  return len2

foreign export ccall "machine_states" machineStates :: Ptr MachineC -> IO (Ptr [StateC])
machineStates machinePtr = peek $ castPtr machinePtr `plusPtr` ( 2 * sizeOf(undefined :: Int) + sizeOf(undefined :: Ptr (Ptr CString)) )

foreign export ccall "machine_states_i" machineStateI :: Ptr (Ptr StateC) -> Int -> IO (Ptr StateC)
machineStateI = peekElemOff

freeMachine :: Ptr MachineC -> IO()
freeMachine machinePtr = do
  (MachineC _ components _ states) <- peek machinePtr
  forM_ components $ \(compType, compName) -> do
    free compType
    free compName
  forM_ states freeState
  -- Free the array pointers
  componentsArray <- peek (castPtr machinePtr `plusPtr` sizeOf(undefined :: Int))
  statesArray <- peek (castPtr machinePtr `plusPtr`
          ( 2 * sizeOf(undefined :: Int)
          + sizeOf(undefined :: Ptr (Ptr CString)) ))
  free componentsArray
  free statesArray
  free machinePtr

-- Automata functions
foreign export ccall "automata_type" automataType :: Ptr AutomataC -> IO Int
automataType autoPtr = do
  automata <- peek autoPtr
  return $ case automata of
    MachC {} -> 0
    MacroC {} -> 1

foreign export ccall "automata_name" automataName :: Ptr AutomataC -> IO CString
automataName autoPtr = do
  automata <- peek autoPtr
  return $ case automata of
    MachC name _ -> name
    MacroC name _ -> name

foreign export ccall "automata_machine" automataMachine :: Ptr AutomataC -> IO (Ptr MachineC)
automataMachine machinePtr = do
  automata <- peek machinePtr
  return $ case automata of
    MachC _ machine -> machine
    MacroC {} -> undefined

foreign export ccall "automata_macro" automataMacro :: Ptr AutomataC -> IO (Ptr MacroKeywordC)
automataMacro macroPtr = do
  automata <- peek macroPtr
  return $ case automata of
    MachC {} -> undefined
    MacroC _ macro -> macro

foreign export ccall "free_automata" freeAutomata :: Ptr AutomataC -> IO ()
freeAutomata autoPtr = do
  automata <- peek autoPtr
  case automata of
    MachC name machinePtr -> do
      freeMachine machinePtr
      free name
    MacroC name macroPtr -> do
      freeMacro macroPtr
      free name
  free autoPtr

-- Program functions
foreign export ccall "result_type" resultType :: Ptr ResultC -> IO Int
resultType resultPtr = do
  result <- peek resultPtr
  return $ case result of
    ProgramC {} -> 0
    ErrorC {} -> 1

foreign export ccall "result_program_len" resultProgramLen :: Ptr ResultC -> IO Int
resultProgramLen resultPtr = do
  result <- peek resultPtr
  return $ case result of
    ProgramC len _ -> len
    ErrorC {} -> undefined

foreign export ccall "program_automata" programAutomata :: Ptr ResultC -> IO (Ptr [AutomataC])
programAutomata programPtr = peek $ castPtr programPtr `plusPtr` (2 * sizeOf (undefined :: Int))

foreign export ccall "program_automata_i" programAutomataI :: Ptr (Ptr AutomataC) -> Int -> IO (Ptr AutomataC)
programAutomataI = peekElemOff

foreign export ccall "error_string" resultError :: Ptr ResultC -> IO CString
resultError resultPtr = do
  result <- peek resultPtr
  return $ case result of
    ProgramC _ _ -> undefined
    ErrorC error _ _ -> error 

foreign export ccall "error_line" resultErrorLine :: Ptr ResultC -> IO Int
resultErrorLine resultPtr = do
  result <- peek resultPtr
  return $ case result of
    ProgramC _ _ -> undefined
    ErrorC _ line _ -> line

foreign export ccall "error_column" resultErrorColumn :: Ptr ResultC -> IO Int
resultErrorColumn resultPtr = do
  result <- peek resultPtr
  return $ case result of
    ProgramC _ _ -> undefined
    ErrorC _ _ column -> column

foreign export ccall "free_result" freeResult :: Ptr ResultC -> IO ()
freeResult resultPtr = do
  result <- peek resultPtr
  case result of
    ProgramC _ automataList -> do
      forM_ automataList freeAutomata
      automataArray <- peek (castPtr resultPtr `plusPtr` (2 * sizeOf(undefined :: Int)))
      free automataArray
    ErrorC error _ _ -> free error
  free resultPtr

-- Test functions
foreign export ccall "test_transition" testTransition :: Int -> IO (Ptr TransitionC)
testTransition n = new.encryptTransition $
  map (\tr -> let (Just (_, Right transition)) = runParser transitionPE (Leftover tr 0 0) in transition)
  [transition1, transition2]
  !! (n - 1)

foreign export ccall "test_state" testState :: Int -> IO (Ptr StateC)
testState n = new.encryptState $
  map(\st -> let (Just (_, Right state)) = runParser statePE (Leftover st 0 0) in state)
  [state1, state2, state3, state4]
  !! (n - 1)

foreign export ccall "test_macro" testMacro :: Int -> IO (Ptr AutomataC)
testMacro n = new.encryptAutomata $
  map(\mc -> let (Just (_, Right macro)) = runParser macroPE (Leftover mc 0 0) in macro)
  [macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8, macro9, macro10, macro11]
  !! (n - 1)

foreign export ccall "test_machine" testMachine :: Int -> IO (Ptr AutomataC)
testMachine n = new.encryptAutomata $
  map(\mc -> let (Just (_, Right machine)) = runParser machinePE (Leftover mc 0 0) in machine)
  [machine1, machine2, machine3]
  !! (n - 1)

foreign export ccall "test_program" testProgram :: Int -> IO (Ptr ResultC)
testProgram n = new.runAmethystParser $ [program1, program2, program3] !! (n - 1)

foreign export ccall "clean_up" cleanUp :: IO ()
cleanUp = System.Mem.performGC
