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
import Michelson.Interpret
import Michelson.Test.Dummy

import Prelude hiding ((>>), and, show, unwords, swap, drop, get, some)
import Text.Show

import Lorentz.Contracts.Permit.Type

-- | Given a `Contract` that requires a "`sender` check"-like
-- operation, this adds functionality so that a user can pre-approve parameters,
-- allowing others to submit those parameters on their behalf
permitWrapperContract :: forall cp st. (HasTypeAnn cp, NiceParameterFull cp)
  => cp & (Parameter cp, Storage st) & '[] :-> ([Operation], Storage st) & '[]
  -> ContractCode (Parameter cp) (Storage st)
permitWrapperContract targetContract = do
  dup
  car
  caseT @(Parameter cp)
    ( #cPermit /-> do
        permitParam @cp @_ @st
        nil
        pair
    , #cWrapped /-> targetContract
    )

assertSignature :: PublicKey & Signature & ByteString & s :-> s
assertSignature = do
  dip $ dip dup
  checkSignature
  if Holds
     then drop
     else do
       push $ mkMTextUnsafe "missigned"
       pair
       failWith

checkPermit :: forall cp t st s. (HasTypeAnn cp, NiceParameterFull cp) =>
  SignedParams & (t, Storage st) & s :-> Permit & Storage st & s
checkPermit = do
  unSignedParams
  dup
  car
  dup
  dip $ do
    swap
    cdr
    dup
    car
    dip $ do
      cdr
      dup
      dip $ do
        dip publicKeyToAddress
        pair
        swap
      swap
      cdr
      dup
      unStorage
      cdr
      car
      dip $ do
        swap
      packWithChainId @(Parameter cp)
  assertSignature
  swap
  toPermit

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

addPermit :: Permit & Storage st & s :-> Storage st & s
addPermit = do
  dip $ do
    unStorage
    dup
    cdr
    incrementCounter
    swap
    car
    unit
    some
  update
  pair
  toStorage

permitParam :: forall cp t st s. (HasTypeAnn cp, NiceParameterFull cp)
  => SignedParams & (t, Storage st) & s :-> Storage st & s
permitParam = do
  checkPermit @cp
  addPermit

packWithChainId
    :: forall p s. NiceParameterFull p
    => ("counter" :! Natural) & Blake2B & s :-> ByteString & s
packWithChainId = do
  pair
  selfCalling @p CallDefault
  address
  chainId
  pair
  pair
  pack @((ChainId, Address), ("counter" :! Natural, Blake2B))

-- | Interpret Michelson code and generate corresponding bytestring.
runPackWithChainId :: forall p. NiceParameterFull p => ChainId -> Address -> ("counter" :! Natural) -> Blake2B -> ByteString
runPackWithChainId chainId' selfAddress' counter' bytes' = either
    (error . fromString . show)
    (\case {Identity ys :& RNil -> ys})
    $ interpretLorentzInstr
      env
      (packWithChainId @p @'[])
      (Identity counter' :& Identity bytes' :& RNil)
  where
    env = dummyContractEnv { ceSelf = selfAddress', ceChainId = chainId' }


-- | Convert a `PublicKey` to an `Address`
publicKeyToAddress :: PublicKey & s :-> Address & s
publicKeyToAddress = do
  hashKey
  implicitAccount
  address

