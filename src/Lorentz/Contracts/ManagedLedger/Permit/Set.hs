
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.ManagedLedger.Permit.Set
  ( module Lorentz.Contracts.ManagedLedger.Permit.Set.Type
  , module Lorentz.Contracts.ManagedLedger.Permit.Set.Impl

  , permittableManagedLedgerSetContract
  , printPermittableManagedLedgerSetContract
  ) where

import Lorentz.Contracts.ManagedLedger.Permit.Set.Type
import Lorentz.Contracts.ManagedLedger.Permit.Set.Impl
import qualified Lorentz.Contracts.Permit.Set.Type as PermitSet

import Data.Bool
import Data.Function
import Data.Maybe
import System.IO

-- import Tezos.Crypto (checkSignature)
import Lorentz -- hiding (checkSignature)
import Michelson.Text
import Util.IO
import Util.Named

import Tezos.Crypto.Orphans ()
import qualified Lorentz.Contracts.Permit.Set as Permit
import qualified Lorentz.Contracts.Permit.Set.Type as Permit
-- import qualified Lorentz.Contracts.Revoke as Revoke


import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Base16 as Base16

-- forall t s.
  -- Parameter & (t, Permit.Storage Storage) & s :-> ([Operation], Permit.Storage Storage) & s
permittableManagedLedgerSetContract :: Contract Parameter (Storage PermitSet.PermitSet)
permittableManagedLedgerSetContract = managedLedgerContractTemplate @PermitSet.PermitSet

-- | Print `permitAdmin42Contract`
--
-- @
--  printPermitAdmin42 (Just "contracts/permit_admin_42.tz") False
-- @
printPermittableManagedLedgerSetContract :: Maybe FilePath -> Bool -> IO ()
printPermittableManagedLedgerSetContract mOutput forceOneLine' =
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine' $
      permittableManagedLedgerSetContract
      -- (defaultContract permittableManagedLedgerSetContract)
        { cDisableInitialCast = True }


  -- :: forall permits store. StorageC permits store => Contract Parameter store

-- permittableAdmin42SetContract :: forall t s.
--   Natural & (t, Permit.Storage Address) & s :-> ([Operation], Permit.Storage Address) & s
--      -- ContractCode Natural (Permit.Storage Address)
-- permittableAdmin42SetContract = do
--   -- unpair
--   dup
--   push @Natural 42
--   assertEq $ mkMTextUnsafe "not 42"
--   pack
--   Permit.safeBlake2B
--   dip $ do
--     cdr
--     Permit.unStorage
--     dup
--     car
--     dip $ do
--       cdr
--       dup
--       cdr
--   swap
--   dip $ do
--     pair
--     Permit.toPermit
--   Permit.assertSentParam
--   pair
--   Permit.toStorage
--   nil
--   pair

