{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Set
  ( module Lorentz.Contracts.Permit.Set
  , runPackWithChainId
  ) where

import Lorentz hiding (concat)
import Michelson.Text
import Michelson.Interpret
import Michelson.Test.Dummy

import Prelude hiding ((>>), and, show, unwords, swap, drop, get, some)
import Text.Show

import Lorentz.Contracts.Permit
import qualified Lorentz.Contracts.Permit.Type as Permit
import Lorentz.Contracts.Permit.Set.Type

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

permitParam :: forall cp t st s. (HasTypeAnn cp, NiceParameterFull cp)
  => SignedParams & (t, Storage st) & s :-> Storage st & s
permitParam = do
  checkPermit @cp
  addPermit

