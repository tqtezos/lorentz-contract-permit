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
  => ContractCode (CheckSentParam cp) (Storage st)
  -> ContractCode (Parameter cp) (Storage st)
permitWrapperContract targetContract = do
  unpair
  caseT @(Parameter cp)
    ( #cPermitParam /-> do
        permitParam @cp @st
        nil
        pair
    , #cWrappedParam /-> do
        lambda assertSentParam
        pair
        toCheckSentParam
        pair
        targetContract
    )

getCounter :: Storage st & s :-> ("counter" :! Natural) & Storage st & s
getCounter = do
  dup
  unStorage
  cdr
  car

assertSignature :: PublicKey & Signature & ByteString & s :-> s
assertSignature = do
  dip $ dip dup --
  checkSignature
  -- assert $ mkMTextUnsafe "missigned"
  if Holds
     then drop
     else do
       push $ mkMTextUnsafe "missigned"
       pair
       failWith

checkPermit :: forall cp st s. (HasTypeAnn cp, NiceParameterFull cp) =>
  SignedParams & Storage st & s :-> Permit & Storage st & s
checkPermit = do
  unSignedParams
  unpair
  dip $ do
    dip getCounter
    unpair
    dip $ do
      dup
      dig @2
      packWithChainId @(Parameter cp)
  dup
  dip $ do
    assertSignature
  publicKeyToAddress
  swap
  pair
  toPermit

incrementCounter :: ("counter" :! Natural) & s :-> ("counter" :! Natural) & s
incrementCounter = do
  forcedCoerce_ @("counter" :! Natural) @Natural
  push @Natural 1
  add
  forcedCoerce_ @Natural @("counter" :! Natural)

addPermit :: Permit & Storage st & s :-> Storage st & s
addPermit = do
  dip $ do
    unStorage
    unpair
  addToPermits
  dip $ do
    unpair
    incrementCounter
    pair
  pair
  toStorage

permitParam :: forall cp st s. (HasTypeAnn cp, NiceParameterFull cp)
  => SignedParams & Storage st & s :-> Storage st & s
permitParam = do
  checkPermit @cp
  addPermit

-- -- | Pack the bytes of the chain id with the current contract address
-- hashWithChainId
--     :: forall p s. NiceParameterFull p
--     => ("counter" :! Natural) & ByteString & s :-> ByteString & s
-- hashWithChainId = do
--   dip safeBlake2B
--   pair
--   stackType @(("counter" :! Natural, Blake2B) & s)
--   packWithChainId @p

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

