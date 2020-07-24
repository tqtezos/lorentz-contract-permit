
module Lorentz.Contracts.ManagedLedger.Permit.Set.Type where

import Lorentz
import Lorentz.Contracts.Spec.ApprovableLedgerInterface (GetAllowanceParams)
import Michelson.Typed (Notes(..))
import Michelson.Untyped (ann, noAnn)

import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger

tt :: ()
tt = _

data StorageSkeleton permits fields = StorageSkeleton
  { ledger    :: BigMap Address LedgerValue
  , permits   :: BigMap Address permits
  , fields    :: fields
  } deriving stock Generic
    deriving anyclass IsoValue

instance (StoreHasField fields fname ftype, IsoValue fields) =>
    StoreHasField (StorageSkeleton fields) fname ftype where
  storeFieldOps = storeFieldOpsDeeper #fields

instance IsoValue fields =>
    StoreHasSubmap (StorageSkeleton permits fields) "ledger" Address LedgerValue where
  storeSubmapOps = storeSubmapOpsDeeper #ledger

instance IsoValue fields =>
    StoreHasSubmap (StorageSkeleton permits fields) "permits" GetAllowanceParams Natural where
  storeSubmapOps = storeSubmapOpsDeeper #permits

data StorageFields fields = StorageFields
  { storageFields :: fields
  , ledgerFields  :: ManagedLedger.StorageFields
  } deriving stock Generic
    deriving anyclass IsoValue

instance HasFieldOfType (StorageFields fields) name field =>
         StoreHasField StorageFields name field where
  storeFieldOps = storeFieldOpsADT

type Storage permits fields = StorageSkeleton permits (StorageFields fields)

type StorageC permits fields store =
  ( LedgerC permits store
  , StorageContains store
   [ "admin" := Address
   , "paused" := Bool
   , "fields" := fields
   , "permits" := Address ~> permits
   ]
  )

type LedgerC permits store = StorageContains store
  [ "totalSupply" := Natural
  , "ledger" := Address ~> LedgerValue
  -- , "approvals" := GetAllowanceParams ~> Natural
  ]

