{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Revoke where

import Lorentz
import Michelson.Text

import Prelude hiding ((>>), and, show, unwords, swap, drop, get, some)

import Lorentz.Contracts.Permit.Type (Blake2B)
-- import Lorentz.Contracts.Permit.Type (Blake2B, CheckSentParam(..), Storage(..))
-- import qualified Lorentz.Contracts.Permit.Type as Permit

data RevokeParams = RevokeParams
  { paramHash :: !Blake2B
  , user :: !Address
  }
  deriving stock Generic

deriving stock instance Show RevokeParams
deriving anyclass instance IsoValue RevokeParams
instance HasTypeAnn RevokeParams

unRevokeParams :: forall s. RevokeParams & s :-> (Blake2B, Address) & s
unRevokeParams = forcedCoerce_

data Parameter cp
  = Revoke !RevokeParams
  | WrappedParam !cp
  deriving stock Generic

deriving stock instance (Show cp) => Show (Parameter cp)
deriving anyclass instance (IsoValue cp) => IsoValue (Parameter cp)
instance HasTypeAnn cp => HasTypeAnn (Parameter cp)

instance (HasTypeAnn cp, IsoValue cp) => ParameterHasEntryPoints (Parameter cp) where
  type ParameterEntryPointsDerivation (Parameter cp) = EpdPlain

-- -- | Add an entrypoint to revoke a permit
-- revokeWrapperContract :: forall cp st. (IsoValue cp)
--   => ContractCode (CheckSentParam cp) (Storage st)
--   -> ContractCode (CheckSentParam (Parameter cp)) (Storage st)
-- revokeWrapperContract targetContract = do
--   unpair
--   Permit.unCheckSentParam
--   unpair
--   swap
--   caseT @(Parameter cp)
--     ( #cRevoke /-> do
--         unRevokeParams
--         dup
--         dip $ do
--           -- TODO: fix ByteString vs. Blake2B
--           _ -- dig @2
--           Permit.unStorage
--           unpair
--           dip swap
--           pair
--           Permit.toSentParam
--           dip swap
--           exec
--           none
--         update
--         pair
--         Permit.toStorage
--         nil >> pair
--     , #cWrappedParam /-> do
--         swap
--         pair
--         Permit.toCheckSentParam
--         pair
--         targetContract
--     )

-- | Assert that the `sender` is `eq` to the given `Address`
assertSender :: Address & s :-> s
assertSender = do
  sender
  assertEq $ mkMTextUnsafe "not your key"

