{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Type where

import Lorentz
import Tezos.Crypto.Hash
import Util.Named
import Michelson.Parser -- hiding (parseValue)
import Michelson.TypeCheck.Instr
import Michelson.TypeCheck.TypeCheck
import Michelson.Macro

import Data.Either
import Data.Function
import Control.Applicative
import Control.Monad hiding ((>>), fail)
import Control.Monad.Fail
import Text.Show (Show(..))

import Control.Monad.Trans.Reader
import Data.Singletons
import Text.Megaparsec (eof)

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

data Permit = MkPermit
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

data Parameter cp
  = Permit !SignedParams
  | Wrapped !cp
  deriving stock Generic

deriving stock instance (Show cp) => Show (Parameter cp)
deriving anyclass instance (IsoValue cp) => IsoValue (Parameter cp)

instance (HasTypeAnn cp, IsoValue cp) => ParameterHasEntryPoints (Parameter cp) where
  type ParameterEntryPointsDerivation (Parameter cp) = EpdPlain

data Storage permits st = Storage
  { presignedParams :: !permits
  , counter         :: !("counter" :! Natural)
  , wrappedStorage  :: !st
  }
  deriving stock Generic

deriving stock instance (Show permit, Show st) => Show (Storage permit st)
deriving anyclass instance (IsoValue permit, IsoValue st) => IsoValue (Storage permit st)

unStorage :: Storage permits st & s :-> (permits, ("counter" :! Natural, st)) & s
unStorage = forcedCoerce_

toStorage :: (permits, ("counter" :! Natural, st)) & s :-> Storage permits st & s
toStorage = forcedCoerce_

incrementCounter :: ("counter" :! Natural, st) & s :-> ("counter" :! Natural, st) & s
incrementCounter = do
  dup
  car
  forcedCoerce_ @("counter" :! Natural) @Natural
  push @Natural 1
  add
  forcedCoerce_ @Natural @("counter" :! Natural)
  dip cdr
  pair

mkStorage :: Monoid permits => st -> Storage permits st
mkStorage = Storage mempty (#counter .! 0)

-- | `Storage` we can parse from the Tezos RPC
data DummyStorage st = DummyStorage
  { dummyPresignedParams :: !Natural
  , counter              :: !("counter" :! Natural)
  , wrappedStorage       :: !st
  }
  deriving stock Generic

deriving stock instance (Show st) => Show (DummyStorage st)
deriving anyclass instance (IsoValue st) => IsoValue (DummyStorage st)

fromDummyStorage :: forall permits st. Monoid permits => DummyStorage st -> Storage permits st
fromDummyStorage DummyStorage{..} =
  Storage mempty counter wrappedStorage

-- | Parse and typecheck a Michelson value
parseTypeCheckValue ::
     forall t. (SingI t)
  => Parser (Value t)
parseTypeCheckValue =
  (>>= either (fail . show) return) $
  runTypeCheckIsolated . flip runReaderT def . typeCheckValue . expandValue <$>
  (value <* eof)

withAdmin42Storage :: forall permit t r. (Monoid permit, IsoValue t, SingI t) => Text -> (Storage permit t -> r) -> r
withAdmin42Storage storageTxt f =
  f . fromDummyStorage $ fromVal @(DummyStorage t) param
  where
    parsedParam = parseNoEnv
      (parseTypeCheckValue @(ToT (DummyStorage t)))
      "Permit DummyStorage"
      storageTxt
    param = either (error . fromString . show) id parsedParam

