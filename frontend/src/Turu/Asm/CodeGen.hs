{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Turu.Asm.CodeGen where

import Turu.Prelude

import qualified Data.Text as T

import Turu.AST
import Turu.AST.Name
import Turu.AST.Var

-- import Turu.Asm.Instr
import GHC.CmmToAsm.X86.Ppr as GHC
import GHC.Types.Name as GHC
import GHC.Types.Name.Occurrence as GHC
import GHC.Cmm.CLabel as GHC
import GHC as GHC
import GHC.Unit.Types as GHC
import GHC.Types.Unique as GHC
import GHC.Data.FastString as GHC
import GHC.Utils.Outputable as GHC
import GHC.SysTools ( initSysTools )
import GHC.Platform as GHC
import GHC.IO.Unsafe
import GHC.Settings
import GHC.Paths

import GHC.CmmToAsm.Format
import GHC.CmmToAsm.X86.Instr as GHC
import GHC.CmmToAsm.X86.Regs as GHC

varToGhcName :: Var -> SrcSpan -> GHC.Name
varToGhcName (MkVar { v_unique , v_name, v_info}) =
    let occ = mkOccName varName (T.unpack $ n_name v_name)
        uniq = (mkUniqueGrimily v_unique)
        mod = unitNameToGhcModule (fromMaybe builtinUnitName $ n_unit v_name)
    in
    -- mkSystemName (mkUniqueGrimily v_unique) (occ)
    mkExternalName uniq mod occ

sillyUnitId :: UnitId
sillyUnitId = UnitId (fsLit "turu-unit-id")

sillyUnit :: GenUnit UnitId
sillyUnit = RealUnit (Definite sillyUnitId)

unitNameToGhcModule :: UnitName -> GHC.Module
unitNameToGhcModule (UnitName {un_name}) =
    mkModule sillyUnit (mkModuleName $ T.unpack un_name)

varLabel :: Var -> GHC.CLabel
varLabel var =
    let lbl_fs = (fsLit $ T.unpack $ n_name $ v_name var)
    in
    mkCmmCodeLabel sillyUnitId lbl_fs

{-# NOINLINE getPlatform #-}
getPlatform :: Platform
getPlatform = unsafePerformIO $ do
    settings <- initSysTools libdir
    return $ sTargetPlatform settings

genAsm :: [Instr] -> SDoc
genAsm instrs =
    vcat (map (pprInstr getPlatform) instrs)
    -- pprBasicBlock config mempty (BasicBlock)

showSDoc :: SDoc -> String
showSDoc = showSDocUnsafe

showAdd :: String
showAdd =
    showSDoc $ genAsm [ADD II64 (OpReg rax) (OpImm $ ImmInt 0)]


-- instruction monad
data IMState = IMState { im_instrs :: [Instr]}
newtype IM a = IM { unIM :: IMState -> (IMState, a) }
    deriving Functor

instance Applicative IM where
    pure x = IM (\s -> (s,x))
    (IM t1) <*> (IM t2) = IM $ \s -> case t1 s of
        (s',f) -> case t2 s' of
            (s'',x) -> (s'',f x)

instance Monad IM where
    (IM t) >>= f = IM $ \s -> case t s of
        (s',x) -> case f x of
            IM t' -> case t' s' of
                (s'',x') -> (s'',x')

gets :: (IMState -> a) -> IM a
gets f = IM $ \s -> (s, f s)

put :: IMState -> IM ()
put s = IM $ \_ -> (s,())


emitInstrs :: [Instr] -> IM ()
emitInstrs instrs = pure ()

codeGen :: CompilationUnit Var -> IM ()
codeGen Unit{unit_binds = binds} = do
    mapM_ cgBind binds

cgBind :: Bind Var -> IM [Instr]
cgBind (Bind ident expr) = undefined

cgExpr :: Expr Var -> IM [Instr]
cgExpr = undefined