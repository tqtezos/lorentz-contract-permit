
module Lorentz.Contracts.ManagedLedger.Permit.Set.Type where

import Lorentz
import Lorentz.Contracts.Spec.ApprovableLedgerInterface (GetAllowanceParams)
import Michelson.Typed (Notes(..))
import Michelson.Untyped (ann, noAnn)

import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import Lorentz.Contracts.Spec.AbstractLedgerInterface (TransferParams(..))
import qualified Lorentz.Contracts.Spec.AbstractLedgerInterface as Abstract

-- tt :: ()
-- tt = _


-- BEGIN not exported from Lorentz.Contracts.ManagedLedger.Types

data StorageFields = StorageFields
  { admin       :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  , counter     :: ("counter" :! Natural)
  } deriving stock Generic
    deriving anyclass IsoValue

instance HasFieldOfType StorageFields name field =>
         StoreHasField StorageFields name field where
  storeFieldOps = storeFieldOpsADT

-- END not exported from Lorentz.Contracts.ManagedLedger.Types

type MintParams =
  ("to" :! Address, "value" :! Natural)

type BurnParams =
  ("from" :! Address, "value" :! Natural)

data Parameter
  = Transfer         AL.TransferParams
  | GetBalance       AL.GetBalanceArg
  | GetTotalSupply   AL.GetTotalSupplyArg
  | SetPause         Bool
  | SetAdministrator Address
  | GetAdministrator (View () Address)
  | Mint             MintParams
  | Burn             BurnParams
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdPlain

type ParameterC param =
  ParameterContainsEntryPoints param
    [ "Transfer"         :> AL.TransferParams
    -- , "Approve"          :> AL.ApproveParams
    -- , "ApproveCAS"       :> ApproveCasParams
    -- , "GetAllowance"     :> AL.GetAllowanceArg
    , "GetBalance"       :> AL.GetBalanceArg
    , "GetTotalSupply"   :> AL.GetTotalSupplyArg
    , "SetPause"         :> Bool
    , "SetAdministrator" :> Address
    , "GetAdministrator" :> (View () Address)
    , "Mint"             :> MintParams
    , "Burn"             :> BurnParams
    ]

-- END parameter types

data StorageSkeleton permits fields = StorageSkeleton
  { ledger    :: BigMap Address ManagedLedger.LedgerValue
  , permits   :: BigMap Address permits
  , fields    :: fields
  } deriving stock Generic
    deriving anyclass IsoValue

instance (StoreHasField fields fname ftype, IsoValue permits, IsoValue fields) =>
    StoreHasField (StorageSkeleton permits fields) fname ftype where
  storeFieldOps = storeFieldOpsDeeper #fields

instance (IsoValue permits, IsoValue fields) =>
    StoreHasSubmap (StorageSkeleton permits fields) "ledger" Address ManagedLedger.LedgerValue where
  storeSubmapOps = storeSubmapOpsDeeper #ledger

instance (IsoValue permits, IsoValue fields) =>
    StoreHasSubmap (StorageSkeleton permits fields) "permits" Address permits where
  storeSubmapOps = storeSubmapOpsDeeper #permits

type Storage permits = StorageSkeleton permits StorageFields

type StorageC permits store =
  ( LedgerC store
  , StorageContains store
   [ "admin" := Address
   , "paused" := Bool
   -- , "fields" := ManagedLedger.LedgerValue
   , "permits" := Address ~> permits
   , "counter" := ("counter" :! Natural)
   ]
  )

type LedgerC store = StorageContains store
  [ "totalSupply" := Natural
  , "ledger" := Address ~> ManagedLedger.LedgerValue
  ]

