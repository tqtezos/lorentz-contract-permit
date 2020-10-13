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

