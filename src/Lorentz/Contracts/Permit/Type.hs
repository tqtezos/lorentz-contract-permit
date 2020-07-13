{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Type where

import Lorentz
import Michelson.Text

import Text.Show (Show(..))

type Blake2B = ByteString

data SignedParams = SignedParams
  { signer_key :: !PublicKey
  , signature  :: !Signature
  , param_hash :: !Blake2B
  }
  deriving stock Show
  deriving stock Generic
  deriving anyclass IsoValue

unSignedParams :: SignedParams & s :-> (PublicKey, (Signature, Blake2B)) & s
unSignedParams = forcedCoerce_

type Permits = BigMap (Blake2B, Address) ()

data SentParam = SentParam
  { approvalMap :: !Permits
  , packedParam :: !Blake2B
  , permitter :: !Address
  }
  deriving stock Generic

deriving stock instance Show SentParam
deriving anyclass instance IsoValue SentParam
instance HasTypeAnn SentParam

unSentParam :: forall s. SentParam & s :-> (Permits, (Blake2B, Address)) & s
unSentParam = forcedCoerce_

toSentParam :: forall s. (Permits, (Blake2B, Address)) & s :-> SentParam & s
toSentParam = forcedCoerce_

-- | Assert the parameter exists and deletes it from `Permits`
assertSentParam :: forall s. SentParam & s :-> Permits & s
assertSentParam = do
  unSentParam
  unpair
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
  = Permit !SignedParams
  | WrappedParam !cp
  deriving stock Generic

deriving stock instance (Show cp) => Show (Parameter cp)
deriving anyclass instance (IsoValue cp) => IsoValue (Parameter cp)

data Storage st = Storage
  { presignedParams :: !Permits
  , counter         :: !Natural
  , wrappedStorage  :: !st
  }
  deriving stock Generic

unStorage :: Storage st & s :-> (Permits, (Natural, st)) & s
unStorage = forcedCoerce_

toStorage :: (Permits, (Natural, st)) & s :-> Storage st & s
toStorage = forcedCoerce_

deriving stock instance (Show st) => Show (Storage st)
deriving anyclass instance (IsoValue st) => IsoValue (Storage st)

