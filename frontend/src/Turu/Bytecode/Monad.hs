{-# LANGUAGE MagicHash #-}

-- | Covers things only needed for the creation of bytecode, but not neccesarily contained within bytecode.
module Turu.Bytecode.Monad where

import Turu.AST
import Turu.AST.Name
import Turu.Bytecode.Types
import Turu.Prelude

import Control.Monad.State.Strict hiding (state)
import Data.Int
import GHC.Exts

import Control.Applicative
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)

data CompilerState = CompilerState
    { cs_stackUse :: !StackUse
    , cs_global_env :: M.Map Name VmConstant
    , cs_anon_constants :: M.Map Int VmConstant
    , cs_arg_env :: M.Map Name Int
    , cs_closure_env :: M.Map Name ClosureOffset
    , cs_local_env :: M.Map Name Int
    , cs_sp :: Int
    -- , cs_next_env_offset :: Int
    }

type CM a = State CompilerState a
type Instructions = [Instruction]

type Offset = Int
type ClosureOffset = Int
data VarPos = ArgPos {var_offset :: Int} | ClosurePos {var_offset :: Int} | GlobalPos DataRef

initCompilerState :: CompilerState
initCompilerState =
    CompilerState
        { cs_stackUse = Bounded 0
        , -- maps constants (named or not) to references
          cs_global_env = mempty
        , cs_anon_constants = mempty
        , -- maps fvs to closure slots
          cs_closure_env = mempty
        , -- maps args to slots
          cs_arg_env = mempty
        , -- maps local variables to slots
          cs_local_env = mempty
        , -- points at the next free stackslot
          cs_sp = 0
        }

runCM :: Maybe CompilerState -> CM a -> (a, CompilerState)
runCM m_state act = runState act (fromMaybe initCompilerState m_state)

cm_getGlobals :: CM (M.Map Name VmConstant)
cm_getGlobals = cs_global_env <$> get

state_addAnonConstant :: CompilerState -> VmConstant -> (CompilerState, Int)
state_addAnonConstant state val =
    -- TODO: We could try to avoid string duplication here.
    let constants = cs_anon_constants state
        next = maybe 0 (succ . fst . fst) (M.maxViewWithKey constants) :: Int
     in (state{cs_anon_constants = M.insert next val constants}, next)

-- Adds a var to the closure env
state_addVarPos :: CompilerState -> Name -> (CompilerState, VarPos)
state_addVarPos state name =
    let v_map = cs_closure_env state
        next
            | null v_map = 0
            -- TODO:Horrible, should keep track of highest offset explicitly
            | otherwise = maximum v_map + 1
     in (state{cs_closure_env = M.insert name next v_map}, ClosurePos next)

addAnonConstantM :: VmConstant -> CM DataRef
addAnonConstantM val = do
    s <- get
    let (s', ref) = state_addAnonConstant s val
    put s'
    return $ AnonRef ref

addGlobalM :: Name -> VmConstant -> CM DataRef
addGlobalM name val = do
    s <- get
    globals <- cm_getGlobals
    assert (not $ M.member name globals) "Error: Global already in scope" $ put s{cs_global_env = M.insert name val globals}
    return $ NameRef name

newClosureVarM :: Name -> CM VarPos
newClosureVarM name = do
    s <- get
    let (s', ref) = state_addVarPos s name
    put s'
    return $ ref

-- Keep track of stack usage.
addStack, subStack, modStack :: Int -> CM ()
addStack = modStack
subStack n = modStack (-n)
modStack n = do
    s <- get
    put s{cs_sp = cs_sp s + n}

incStack, decStack :: CM ()
incStack = modStack 1
decStack = modStack (-1)
getStack :: CM Int
getStack = cs_sp <$> get

getVarPosM :: HasCallStack => Name -> CM VarPos
getVarPosM name = do
    s <- get
    let m_offset =
            ClosurePos <$> (M.lookup name (cs_closure_env s))
                <|> ArgPos <$> (M.lookup name (cs_arg_env s))
                <|> GlobalPos <$> (if M.member name (cs_global_env s) then Just (NameRef name) else Nothing)
        offset = fromMaybe (error $ "Name not found:" <> show name) m_offset
    return $ offset

-- Assign a slot in the closure for all the given vars
-- and compile a functions body. Return the result of
-- compiling the body and the actually needed slots in
-- the closure.
-- NB: Every non-fv variable can be allocated on the stack
-- so we might decide not to use the int part of the result
withClosure ::
    [Var] ->
    [Var] ->
    CM a ->
    CM
        ( a
        , Int {-closure size -}
        , Int {-number of locals-}
        )
withClosure args fv_vars gen_body = do
    let var_map = M.fromList $ zip (fmap getName fv_vars) [0 ..]
    let arg_map = M.fromList $ zip (fmap getName args) [0 ..]
    s <- get
    let old_env = cs_closure_env s
    let old_args = cs_arg_env s
    let old_locals = cs_local_env s
    put $ s{cs_closure_env = var_map, cs_arg_env = arg_map, cs_local_env = mempty}
    r <- gen_body
    s1 <- get
    let !slots = M.size $ cs_closure_env s1
    let !n_locals = M.size $ cs_local_env s1
    put $ s1{cs_closure_env = old_env, cs_arg_env = old_args, cs_local_env = old_locals}
    return (r, slots, n_locals)

-- Add a entry in the locals mapping
addLocalVar :: Var -> CM Int
addLocalVar var = do
    s <- get
    let (s', index) = state_addLocal s
    put s'
    pure index
  where
    name = getName var
    state_addLocal :: CompilerState -> (CompilerState, Int)
    state_addLocal state =
        let locals = cs_local_env state
            next
                | null locals = 0
                | otherwise = maximum locals + 1
         in (state{cs_local_env = M.insert name next locals}, next)

----------

pushVar :: HasCallStack => Var -> CM Instructions
pushVar var = do
    var_pos <- getVarPosM (getName var)
    case var_pos of
        ClosurePos offset -> pure $ [GetClosureVar offset]
        ArgPos offset -> pure $ [GetClosureVar offset]
        GlobalPos ref -> pure $ [PushRef ref]

------------------------