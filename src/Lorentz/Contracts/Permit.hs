{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit where

import Lorentz hiding (concat)
import Michelson.Text

import Prelude hiding ((>>), and, show, unwords, swap, drop, get, some)

import Lorentz.Contracts.Permit.Type

-- | Given a `Contract` that requires a "`sender` check"-like
-- operation, this adds functionality so that a user can pre-approve parameters,
-- allowing others to submit those parameters on their behalf
permitCodeWrapperContract :: forall cp st. (HasTypeAnn cp, NiceParameterFull cp)
  => ContractCode (CheckSentParam cp) (Storage st)
  -> ContractCode (Parameter cp) (Storage st)
permitCodeWrapperContract targetContract = do
  unpair
  caseT @(Parameter cp)
    ( #cPermit /-> permitParam @cp @st >> nil >> pair
    , #cWrappedParam /-> do
        lambda assertSentParam
        pair
        toCheckSentParam
        pair
        targetContract
    )

permitParam :: forall cp st s. (HasTypeAnn cp, NiceParameterFull cp)
  => SignedParams & Storage st & s :-> Storage st & s
permitParam = do
  unSignedParams
  unpair
  dup
  dip $ do
    dip $ do
      unpair
      dip $ do
        dup
        dig @2
        unStorage
        unpair
        swap
        unpair
        forcedCoerce_ @Natural @("counter" :! Natural)
        dup
        dug @5 -- _
        dip $ dig @3
        pair
        packWithChainId @ByteString @_ @(Lambda SentParam Permits, cp)
    checkSignature
    assert $ mkMTextUnsafe "missigned"
  publicKeyToAddress
  dig @3
  pair
  dip $ do
    swap
    unit
    some
  update
  dip $ do
    swap
    forcedCoerce_ @("counter" :! Natural) @Natural
    push @Natural 1
    add
    pair
  pair
  toStorage

-- | Pack the bytes of the chain id with the current contract address
packWithChainId
    :: forall a s p. (NicePackedValue a, NiceParameterFull p)
    => (("counter" :! Natural, a) & s) :-> (ByteString & s)
packWithChainId = selfCalling @p CallDefault >> address >> chainId >> pair >> pair >>  pack @((ChainId, Address), ("counter" :! Natural, a))

-- | Convert a `PublicKey` to an `Address`
publicKeyToAddress :: PublicKey & s :-> Address & s
publicKeyToAddress = do
  hashKey
  implicitAccount
  address

