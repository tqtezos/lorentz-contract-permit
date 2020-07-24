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
  -- , unStorage
  -- , toStorage
  -- , mkStorage
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
  -- , mkStorage
  , incrementCounter
  , toPermit
  , unPermit
  -- , toStorage
  , unSignedParams
  -- , unStorage
  )
import qualified Lorentz.Contracts.Permit.Type as Permit

import Text.Show

-- tt :: ()
-- tt = _

newtype SafeExpiry = SafeExpiry { getSafeExpiry :: Natural }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

unSafeExpiry :: SafeExpiry & s :-> Natural & s
unSafeExpiry = forcedCoerce_

newtype Expiry = Expiry { seconds :: Integer }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

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

-- emptyExpiry :: s :-> PermitExpiry & s
-- emptyExpiry = do
--   emptyMap
--   none
--   pair
--   toPermitExpiry

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

data Parameter cp
  = UserExpiry (Maybe SafeExpiry)
  | DefaultExpiry SafeExpiry
  | Wrapped !cp

data Storage st = Storage
  { defaultExpiry :: Expiry
  , permitStorage :: Permit.Storage Permits st
  }
  deriving stock Generic

deriving stock instance (Show st) => Show (Storage st)
deriving anyclass instance (IsoValue st) => IsoValue (Storage st)

toStorage :: (Expiry, Permit.Storage Permits st) & s :-> Storage st & s
toStorage = forcedCoerce_

unStorage :: Storage st & s :-> (Expiry, Permit.Storage Permits st) & s
unStorage = forcedCoerce_


-- -- | Assert the parameter exists and deletes it from `Permits`
-- assertSentParam :: forall s. Permits & Permit & s :-> Permits & s
-- assertSentParam = do
--   undefined
--   -- _

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
      Permit.unStorage
      unpair
  swap
  dip $ do
    insertPermit
    pair
    Permit.toStorage
  pair
  toStorage

