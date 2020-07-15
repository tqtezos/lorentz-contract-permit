{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Type where

import Lorentz
import Michelson.Text
import Tezos.Crypto.Hash
import Util.Named

import Text.Show (Show(..))

newtype Blake2B =
  UnsafeBlake2B ByteString
  deriving stock Show
  deriving stock Eq
  deriving stock Ord
  deriving stock Generic
  deriving anyclass IsoValue

instance HasTypeAnn Blake2B

safeBlake2B :: forall s. ByteString & s :-> Blake2B & s
safeBlake2B = blake2B >> forcedCoerce_ @ByteString @Blake2B

mkBlake2B :: ByteString -> Blake2B
mkBlake2B = UnsafeBlake2B . blake2b


data SignedParams = SignedParams
  { signerKey :: !PublicKey
  , signature  :: !Signature
  , paramHash :: !Blake2B
  }
  deriving stock Show
  deriving stock Generic
  deriving anyclass IsoValue

instance HasTypeAnn SignedParams

unSignedParams :: SignedParams & s :-> (PublicKey, (Signature, Blake2B)) & s
unSignedParams = forcedCoerce_

data Permit = Permit
  { paramHash :: !Blake2B
  , signer :: !Address
  }
  deriving stock Eq
  deriving stock Ord
  deriving stock Show
  deriving stock Generic
  deriving anyclass IsoValue

instance HasTypeAnn Permit

toPermit :: (Blake2B, Address) & s :-> Permit & s
toPermit = forcedCoerce_

type Permits = BigMap Permit ()

-- | Add a `Permit` to `Permits`
addToPermits :: Permit & Permits & s :-> Permits & s
addToPermits = do
  dip $ do
    unit
    some
  update

data SentParam = SentParam
  { approvalMap :: !Permits
  , packedParam :: !ByteString
  , signer :: !Address
  }
  deriving stock Generic

deriving stock instance Show SentParam
deriving anyclass instance IsoValue SentParam
instance HasTypeAnn SentParam

unSentParam :: forall s. SentParam & s :-> (Permits, (ByteString, Address)) & s
unSentParam = forcedCoerce_

toSentParam :: forall s. (Permits, (ByteString, Address)) & s :-> SentParam & s
toSentParam = forcedCoerce_

-- | Assert the parameter exists and deletes it from `Permits`
assertSentParam :: forall s. SentParam & s :-> Permits & s
assertSentParam = do
  unSentParam
  unpair
  dip $ do
    unpair
    safeBlake2B
    pair
    toPermit
  dup
  dig @2
  dup
  dip $ do
    mem
    assert $ mkMTextUnsafe "no permit"
    none
  update

data CheckSentParam cp = CheckSentParam
  { localAssertSentParam :: !(Lambda SentParam Permits)
  , checkedParam :: !cp
  }
  deriving stock Generic

deriving stock instance (Show cp) => Show (CheckSentParam cp)
deriving anyclass instance (IsoValue cp) => IsoValue (CheckSentParam cp)

unCheckSentParam :: forall cp s. CheckSentParam cp & s :-> (Lambda SentParam Permits, cp) & s
unCheckSentParam = forcedCoerce_

toCheckSentParam :: forall cp s. (Lambda SentParam Permits, cp) & s :-> CheckSentParam cp & s
toCheckSentParam = forcedCoerce_

data Parameter cp
  = PermitParam !SignedParams
  | WrappedParam !cp
  deriving stock Generic

deriving stock instance (Show cp) => Show (Parameter cp)
deriving anyclass instance (IsoValue cp) => IsoValue (Parameter cp)

instance (HasTypeAnn cp, IsoValue cp) => ParameterHasEntryPoints (Parameter cp) where
  type ParameterEntryPointsDerivation (Parameter cp) = EpdPlain


data Storage st = Storage
  { presignedParams :: !Permits
  , counter         :: !("counter" :! Natural)
  , wrappedStorage  :: !st
  }
  deriving stock Generic

deriving stock instance (Show st) => Show (Storage st)
deriving anyclass instance (IsoValue st) => IsoValue (Storage st)

unStorage :: Storage st & s :-> (Permits, ("counter" :! Natural, st)) & s
unStorage = forcedCoerce_

toStorage :: (Permits, ("counter" :! Natural, st)) & s :-> Storage st & s
toStorage = forcedCoerce_

mkStorage :: st -> Storage st
mkStorage = Storage mempty (#counter .! 0)

-- -- | Ignore `Storage`
-- ignoreStorage :: ContractCode cp st -> ContractCode cp (Storage st)
-- ignoreStorage xs = do
--   unpair
--   dip $ do
--     unStorage
--     unpair
--     dip $ do
--       unpair
--     dig @2
--   pair
--   dug @2
--   dip $ do
--     dip $ do
--       xs
--       unpair
--       swap
--     pair
--   pair
--   toStorage
--   swap
--   pair

