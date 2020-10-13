{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.ManagedLedger.Permit.Set.Impl where

import Lorentz
import Michelson.Text

import Lorentz.Contracts.ManagedLedger.Permit.Set.Type

import qualified Lorentz.Contracts.Permit.Set.Type as Permit

import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import Lorentz.Contracts.Spec.AbstractLedgerInterface (TransferParams, GetBalanceArg, GetTotalSupplyArg)

----------------------------------------------------------------------------
-- Entrypoints
----------------------------------------------------------------------------

transfer
  :: forall store. (StorageContains store '["permits" := Address ~> Permit.PermitSet], LedgerC store, StoreHasField store "paused" Bool)
  => Entrypoint TransferParams store
transfer = do
  dip ensureNotPaused

  -- Check whether we need to consider permit
  stackType @[TransferParams, store]

  getField #from
  sender
  stackType @[Address, Address, TransferParams, store]
  if IsEq
     then nop -- from == sender
    else do -- we need to check for a permit
      dup
      toField #from
      swap
      dup
      dip $ do
        pack
        Permit.safeBlake2B
        pair
        Permit.toPermit
        Permit.assertSentParamStorageContains

  -- Perform transfer
  debitFrom
  creditTo
  drop @TransferParams
  nil; pair

getBalance :: LedgerC store => Entrypoint GetBalanceArg store
getBalance = view_ $ do
  unpair; fromNamed #owner; stGet #ledger
  ifSome (fromNamed #balance) (push 0)

getTotalSupply :: LedgerC store => Entrypoint GetTotalSupplyArg store
getTotalSupply = do
  view_ (do cdr; stToField #totalSupply)

setPause :: forall permits store. StorageC permits store => Entrypoint Bool store
setPause = do
  dip (authorizeAdmin @permits @store)
  stSetField #paused
  nil; pair

setAdministrator :: forall permits store. StorageC permits store => Entrypoint Address store
setAdministrator = do
  dip (authorizeAdmin @permits @store);
  stackType @[Address, store]
  stSetField #admin
  nil; pair;

getAdministrator :: forall permits store. StorageC permits store => Entrypoint (View () Address) store
getAdministrator = do
  view_ (do cdr; stToField #admin)

mint :: forall permits store. StorageC permits store => Entrypoint MintParams store
mint = do
  dip (authorizeAdmin @permits @store)
  creditTo
  drop @MintParams
  nil; pair

burn :: forall permits store. StorageC permits store => Entrypoint BurnParams store
burn = do
  dip (authorizeAdmin @permits @store)
  debitFrom
  drop @BurnParams
  nil; pair

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

authorizeAdmin ::
  StorageC permits store => store : s :-> store : s
authorizeAdmin = do
  stGetField #admin; sender; eq
  if_ nop (failCustom_ #senderIsNotAdmin)

addTotalSupply
  :: StoreHasField store "totalSupply" Natural
  => Integer : store : s :-> store : s
addTotalSupply = do
  dip $ stGetField #totalSupply
  add; isNat; ifSome nop (failUnexpected [mt|Negative total supply|])
  stSetField #totalSupply

debitFrom
  :: forall param store.
     ( param `HasFieldsOfType` ["from" := Address, "value" := Natural]
     , LedgerC store
     )
  => '[param, store] :-> '[param, store]
debitFrom = do
    -- Get LedgerValue
    duupX @2; duupX @2; toField #from
    stGet #ledger; ifSome nop $ do
      -- Fail if absent
      stackType @[param, store]
      toField #value; toNamed #required; push 0; toNamed #present
      swap; pair; failCustom #notEnoughBalance
    -- Get balance
    stackType @[ManagedLedger.LedgerValue, param, store]
    fromNamed #balance; dup
    duupX @3; toField #value
    rsub; isNat
    ifSome nop $ do
      -- Fail if balance is not enough
      stackType @[Natural, param, store]
      toNamed #present
      duupX @2; toField #value; toNamed #required
      pair; failCustom #notEnoughBalance
    -- Update balance, LedgerValue and Storage
    swap; drop; nonEmptyLedgerValue
    stackType @[Maybe ManagedLedger.LedgerValue, param, store]
    swap; dup; dip $ do
      toField #from
      stUpdate #ledger

    -- Update total supply
    dup; dip $ do toField #value; neg; addTotalSupply

creditTo
  :: ( param `HasFieldsOfType` ["to" := Address, "value" := Natural]
     , LedgerC store
     )
  => '[param, store] :-> '[param, store]
creditTo = do
    -- Get LedgerValue
    duupX @2; duupX @2; toField #to
    stGet #ledger
    if IsSome
      then do -- Get balance
              duupX @2; toField #value; dip (fromNamed #balance)
              add @Natural; toNamed #balance; some
      else do -- Construct LedgerValue (if not empty)
              getField #value; int
              ifEq0 none $ do
                getField #value
                toNamed #balance
                some
    -- Update LedgerValue and Storage
    swap
    dup; dip $ do toField #to; stUpdate #ledger

    -- Update total supply
    dup; dip $ do toField #value; int; addTotalSupply

-- | Ensure that given 'LedgerValue' value cannot be safely removed
-- and return it.
nonEmptyLedgerValue :: Natural : s :-> Maybe ManagedLedger.LedgerValue : s
nonEmptyLedgerValue = do
  dup; int
  if IsZero
  then drop >> none
  else toNamed #balance >> some

ensureNotPaused
  :: StoreHasField store "paused" Bool
  => store : s :-> store : s
ensureNotPaused = do
  stGetField #paused
  if_ (push (mkMTextUnsafe "tokenOperationsArePaused") >> failWith) nop

----------------------------------------------------------------------------
-- Contract
----------------------------------------------------------------------------

-- | defaultContract $ contractName "Managed Ledger with Permit" $
managedLedgerContractTemplate :: forall store. StorageC Permit.PermitSet store => ContractCode Parameter store
managedLedgerContractTemplate = do
  contractGeneralDefault
  unpair
  entryCaseSimple @Parameter
    ( #cTransfer /-> transfer @store
    , #cGetBalance /-> getBalance @store
    , #cGetTotalSupply /-> getTotalSupply @store
    , #cSetPause /-> setPause @Permit.PermitSet @store
    , #cSetAdministrator /-> setAdministrator @Permit.PermitSet @store
    , #cGetAdministrator /-> getAdministrator @Permit.PermitSet @store
    , #cMint /-> mint @Permit.PermitSet @store
    , #cBurn /-> burn @Permit.PermitSet @store
    )

