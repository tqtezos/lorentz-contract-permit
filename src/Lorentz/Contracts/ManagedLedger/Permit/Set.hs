
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
  , initPermittableManagedLedgerSetContract
  , printPermittableManagedLedgerSetParam
  ) where

import Lorentz.Contracts.ManagedLedger.Permit.Set.Type
import Lorentz.Contracts.ManagedLedger.Permit.Set.Impl
import qualified Lorentz.Contracts.Permit.Set.Type as PermitSet

import Data.Bool
import Data.Function
import Data.Maybe
import System.IO

import Lorentz
import Michelson.Text
import Util.IO
import Util.Named

import Tezos.Crypto.Orphans ()
import qualified Lorentz.Contracts.Permit.Set as Permit
import qualified Lorentz.Contracts.Permit.Set.Type as Permit

import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Base16 as Base16

instance ParameterHasEntryPoints (Permit.Parameter Parameter) where
  type ParameterEntryPointsDerivation (Permit.Parameter Parameter) = EpdRecursive

permittableManagedLedgerSetContract :: ContractCode (Permit.Parameter Parameter) (Storage PermitSet.PermitSet)
permittableManagedLedgerSetContract = Permit.permitWrapperContractStorageContains @Parameter $ do
  dip cdr
  pair
  managedLedgerContractTemplate @(Storage PermitSet.PermitSet)

-- permitWrapperContract :: forall cp st. (HasTypeAnn cp, NiceParameterFull cp)
--   => cp & (Parameter cp, Storage st) & '[] :-> ([Operation], Storage st) & '[]
--   -> ContractCode (Parameter cp) (Storage st)

-- | Print `permittableManagedLedgerSetContract`
--
-- @
--  printPermittableManagedLedgerSetContract (Just "contracts/permit_fa1.2_set.tz") False
-- @
printPermittableManagedLedgerSetContract :: Maybe FilePath -> Bool -> IO ()
printPermittableManagedLedgerSetContract mOutput forceOneLine' =
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine' $
      (defaultContract permittableManagedLedgerSetContract)
        { cDisableInitialCast = True }

-- | Unpaused with empty initial ledger
mkSimpleStorage :: Address -> Storage PermitSet.PermitSet
mkSimpleStorage adminAddress =
  StorageSkeleton
    { ledger    = mempty
    , permits   = mempty
    , fields    = StorageFields
      { admin       = adminAddress
      , paused      = False
      , totalSupply = 0
      , counter     = #counter .! 0
      }
    }

initPermittableManagedLedgerSetContract :: Address -> Bool -> IO ()
initPermittableManagedLedgerSetContract adminAddress forceOneLine =
  TL.putStrLn $
  printLorentzValue @(Storage PermitSet.PermitSet) forceOneLine $
  mkSimpleStorage adminAddress

printPermittableManagedLedgerSetParam :: PublicKey -> Signature -> Address -> Address -> Natural -> IO () -- ChainId -> Address -> Natural ->
printPermittableManagedLedgerSetParam key' sig' from' to' value' -- _chainId' _contractAddr' _counter'
  | True = -- checkSignature key' sig' hashBytes'
      let signedParams' = Permit.SignedParams key' sig' hash' in
      -- runPermitAdmin42 chainId' contractAddr' (mkKeyAddress key') signedParams' *>
      TL.putStrLn (
        printLorentzValue @Permit.SignedParams forceOneLine signedParams'
        )
  -- | True = error "missigned"
  where
    forceOneLine = True

    bytes' :: ByteString
    bytes' = lPackValue $ (#from .! from', #to .! to', #value .! value')
    -- ("from" :! Address, "to" :! Address, "value" :! Natural)

    hash' :: Permit.Blake2B
    hash' = Permit.mkBlake2B bytes'

