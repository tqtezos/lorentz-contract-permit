{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Expiry.Type
  ( module Lorentz.Contracts.Permit.Expiry.Type
  , Permit(..)
  , SignedParams(..)
  , Parameter(..)
  , DummyStorage(..)
  , Blake2B
  , mkBlake2B
  , safeBlake2B
  , toPermit
  , unPermit
  , incrementCounter
  , unSignedParams
  ) where

import Lorentz

import Lorentz.Contracts.Permit.Type
  ( Permit(..)
  , SignedParams(..)
  , DummyStorage(..)
  , Blake2B
  , mkBlake2B
  , safeBlake2B
  , incrementCounter
  , toPermit
  , unPermit
  , unSignedParams
  )
import qualified Lorentz.Contracts.Permit.Type as Permit

import GHC.Num
import GHC.Enum
import GHC.Real
import Text.Show

newtype SafeExpiry = SafeExpiry { getSafeExpiry :: Natural }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue
  deriving newtype Eq
  deriving newtype Ord
  deriving newtype Enum
  deriving newtype Num
  deriving newtype Real
  deriving newtype Integral

instance HasTypeAnn SafeExpiry

oneDayExpiry :: SafeExpiry
oneDayExpiry =
  24 * -- hours/day
  60 * -- minutes/hour
  60   -- seconds/minute

unSafeExpiry :: SafeExpiry & s :-> Natural & s
unSafeExpiry = forcedCoerce_

newtype Expiry = Expiry { seconds :: Integer }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue
  deriving newtype Num

unsafeToExpiry :: Integer & s :-> Expiry & s
unsafeToExpiry = forcedCoerce_

safeExpiry :: SafeExpiry & s :-> Expiry & s
safeExpiry = do
  unSafeExpiry
  int
  unsafeToExpiry

unExpiry :: Expiry & s :-> Integer & s
unExpiry = forcedCoerce_

data CreatedAt = CreatedAt
  { created :: !Timestamp
  , expiry  :: !(Maybe Expiry)
  }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

unCreatedAt :: CreatedAt & s :-> (Timestamp, Maybe Expiry) & s
unCreatedAt = forcedCoerce_

toCreatedAt :: (Timestamp, Maybe Expiry) & s :-> CreatedAt & s
toCreatedAt = forcedCoerce_

createdAt :: s :-> CreatedAt & s
createdAt = do
  none
  now
  pair
  toCreatedAt

assertNotExpired :: Timestamp & Expiry & s :-> s
assertNotExpired = do
  swap
  unExpiry
  add
  now
  lt
  assert $ UnspecifiedError -- mkMTextUnsafe "permit expired"

runCreatedAt :: CreatedAt & s :-> Maybe Timestamp & s
runCreatedAt = do
  unCreatedAt
  dup
  cdr
  ifNone
    (do
      car
      some
    )
    (do
      swap
      car
      assertNotExpired
      none
    )

assertCreatedAtExpiry :: Expiry & Maybe Timestamp & s :-> s
assertCreatedAtExpiry = do
  swap
  ifNone
    drop
    assertNotExpired

convertGenericMultisigLambda :: Lambda (Lambda () [Operation], Address) ([Operation], Address)
convertGenericMultisigLambda = do
  unpair
  unit
  exec
  pair

createdAtExpired :: CreatedAt & Expiry & s :-> Bool & s
createdAtExpired = do
  unCreatedAt
  dup
  cdr
  dip $ do
    car
    swap
  ifNone
    nop
    (do
      swap
      drop
    )
  unExpiry
  add
  now
  lt

-- | Unset if zero
setCreatedAtExpiry :: CreatedAt & SafeExpiry & s :-> CreatedAt & s
setCreatedAtExpiry = do
  unCreatedAt
  car
  swap
  safeExpiry
  dup
  unExpiry
  ifEq0
    (do
      drop
      none
    )
    (do
      some
    )
  swap
  pair
  toCreatedAt

--   unCreatedAt

data PermitExpiry = PermitExpiry
  { userExpiry :: !(Maybe Expiry)
  , hashes     :: !(Map Blake2B CreatedAt)
  }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

toPermitExpiry :: (Maybe Expiry, Map Blake2B CreatedAt) & s :-> PermitExpiry & s
toPermitExpiry = forcedCoerce_

unPermitExpiry :: PermitExpiry & s :-> (Maybe Expiry, Map Blake2B CreatedAt) & s
unPermitExpiry = forcedCoerce_

emptyPermitExpiry :: s :-> PermitExpiry & s
emptyPermitExpiry = do
  emptyMap
  none
  pair
  toPermitExpiry

-- | If the given `SafeExpiry` is `0`, set `userExpiry` to `Nothing`
defaultUserPermitExpiry :: SafeExpiry & s :-> PermitExpiry & s
defaultUserPermitExpiry = do
  safeExpiry
  dup
  unExpiry
  ifEq0
    (do
      drop
      emptyPermitExpiry
    )
    (do
      some
      emptyMap
      swap
      pair
      toPermitExpiry
    )

-- | If the given `SafeExpiry` is `0`, set `userExpiry` to `Nothing`
setUserPermitExpiry :: SafeExpiry & PermitExpiry & s :-> PermitExpiry & s
setUserPermitExpiry = do
  safeExpiry
  dup
  unExpiry
  ifEq0
    (do
      drop
      unPermitExpiry
      cdr
      none
    )
    (do
      some
      dip $ do
        unPermitExpiry
        cdr
    )
  pair
  toPermitExpiry

-- | if it doesn't exist, fail
updateParticularPermitsExpiry :: Blake2B & SafeExpiry & PermitExpiry & s :-> PermitExpiry & s
updateParticularPermitsExpiry = do
  dig @2
  unPermitExpiry
  dup
  car
  dip $ do
    cdr
    dup
    dip $ do
      swap
      dup
      dip $ do
        get
        assertSome $ UnspecifiedError -- mkMTextUnsafe "no permit"
        setCreatedAtExpiry
        some
    dug @2
    update @(Map Blake2B CreatedAt)
  pair
  toPermitExpiry

type Permits = BigMap Address PermitExpiry

-- getPermitExpiry :: Address & Permits & s :-> PermitExpiry & s
-- getPermitExpiry = do
--   get
--   ifNone emptyExpiry nop

insertPermit :: Permit & Permits & s :-> Permits & s
insertPermit = do
  unPermit
  dup
  dip $ do
    cdr
    dip dup
    get @Permits
    ifNone
      (none >> emptyMap)
      (do
        unPermitExpiry
        dup
        cdr
        dip car
      )
    createdAt
    some
  dup
  cdr
  dip $ do
    car
    update @(Map Blake2B CreatedAt)
    swap
    pair
    toPermitExpiry
    some
  update @Permits

data ExpiryParams = ExpiryParams
  { permit :: !(Maybe Blake2B)
  , user   :: !Address
  , expiry :: !SafeExpiry
  }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

instance HasTypeAnn ExpiryParams

toExpiryParams :: (Maybe Blake2B, (Address, SafeExpiry)) & s :-> ExpiryParams & s
toExpiryParams = forcedCoerce_

unExpiryParams :: ExpiryParams & s :-> (Maybe Blake2B, (Address, SafeExpiry)) & s
unExpiryParams = forcedCoerce_

data Parameter cp
  = SetExpiry ExpiryParams
  | DefaultExpiry SafeExpiry
  | Wrapped !cp
  deriving stock Generic

deriving stock instance (Show st) => Show (Parameter st)
deriving anyclass instance (IsoValue st) => IsoValue (Parameter st)

data Storage st = Storage
  { defaultExpiry :: !Expiry
  , permitStorage :: !(Permit.Storage Permits st)
  , admin         :: !Address
  }
  deriving stock Generic

deriving stock instance (Show st) => Show (Storage st)
deriving anyclass instance (IsoValue st) => IsoValue (Storage st)

instance (StoreHasField (Permit.Storage Permits st) fname ftype, IsoValue st) =>
    StoreHasField (Storage st) fname ftype where
  storeFieldOps = storeFieldOpsDeeper #permitStorage

instance (IsoValue st) =>
    StoreHasSubmap (Permit.Storage Permits st) "presignedParams" Address PermitExpiry where
  storeSubmapOps = storeSubmapOpsDeeper #presignedParams

mkStorage :: SafeExpiry -> Address -> st -> Storage st
mkStorage defaultExpiry' adminAddress' wrappedState' =
  Storage
    (fromIntegral defaultExpiry')
    (Permit.mkStorage wrappedState')
    adminAddress'

toStorage :: (Expiry, (Permit.Storage Permits st, Address)) & s :-> Storage st & s
toStorage = forcedCoerce_

unStorage :: Storage st & s :-> (Expiry, (Permit.Storage Permits st, Address)) & s
unStorage = forcedCoerce_

updateDefaultExpiry :: KnownValue st => '[SafeExpiry, Storage st] :-> '[Storage st]
updateDefaultExpiry = do
  swap
  dup
  unStorage
  cdr
  cdr
  sender
  ifEq nop failWith
  swap
  safeExpiry
  dip $ do
    unStorage
    cdr
  pair
  toStorage

updatePermitExpiry :: '[Blake2B, (Address, SafeExpiry), Storage st] :-> '[Storage st]
updatePermitExpiry = do
  dip $ do
    dip $ do
      unStorage
      dup
      dip car
      cdr -- unpair >> swap
      unpair
      Permit.unStorage
      unpair
      dup
    dup
    cdr
    dip $ do
      car
      dup
      dip $ do
        get
        assertSome $ UnspecifiedError -- mkMTextUnsafe "no permit"
      swap
  updateParticularPermitsExpiry
  some
  swap
  update
  pair
  Permit.toStorage
  dig @2
  dip pair
  pair
  toStorage

updateUserExpiry :: '[(Address, SafeExpiry), Storage st] :-> '[Storage st]
updateUserExpiry = do
  dip $ do
    unStorage
    dup
    dip $ do
      car
    cdr
    unpair
    Permit.unStorage
    unpair
  dup
  dip $ do
    cdr
    swap
    dup
  car
  dup
  dip $ do
    get
    ifNone
      (do
        swap
        defaultUserPermitExpiry
      )
      (do
        dig @2
        setUserPermitExpiry
      )
    some
  update @Permits
  pair
  Permit.toStorage
  dig @2
  dip pair
  pair
  toStorage



-- -- | Assert the parameter exists and deletes it from `Permits`
-- assertSentParam :: forall s. Permits & Permit & s :-> Permits & s
-- assertSentParam = do
--   --

--   -- swap
--   -- pair
--   -- dup
--   -- unpair
--   -- unPermit
--   -- dup
--   -- cdr
--   -- dip swap
--   -- get @Permits
--   -- if IsNone
--   --    then failWith
--   --    else do
--   --      swap
--   --      dip dup
--   --      car
--   --      mem @PermitSet
--   --      if Holds
--   --         then do
--   --           dip $ do
--   --             unpair
--   --             unPermit
--   --             dup
--   --             car
--   --           swap
--   --           dip $ push False
--   --           update @PermitSet
--   --           some
--   --           swap
--   --           cdr
--   --           update @Permits
--   --         else failWith

addPermit :: forall st s. Permit & Storage st & s :-> Storage st & s
addPermit = do
  dip $ do
    unStorage
    dup
    car
    dip $ do
      cdr
      unpair
      Permit.unStorage
      unpair
  swap
  dip $ do
    insertPermit
    pair
    Permit.toStorage
    pair
  pair
  toStorage

