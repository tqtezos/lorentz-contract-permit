{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Paired.Type
  ( module Lorentz.Contracts.Permit.Paired.Type
  , Permit(..)
  , SignedParams(..)
  , Parameter(..)
  , DummyStorage(..)
  , Blake2B
  , mkBlake2B
  , safeBlake2B
  , toPermit
  , unStorage
  , toStorage
  , mkStorage
  , incrementCounter
  , unSignedParams
  ) where

import Lorentz
import Michelson.Text
import Tezos.Crypto.Hash
import Util.Named

import Text.Show (Show(..))

import Lorentz.Contracts.Permit.Type
  ( Parameter(..)
  , Permit(..)
  , SignedParams(..)
  , DummyStorage(..)
  , Blake2B
  , mkBlake2B
  , safeBlake2B
  , mkStorage
  , incrementCounter
  , toPermit
  , toStorage
  , unSignedParams
  , unStorage
  )
import qualified Lorentz.Contracts.Permit.Type as Permit

type Permits = BigMap Permit ()

type Storage = Permit.Storage Permits

-- | Add a `Permit` to `Permits`
addToPermits :: Permit & Permits & s :-> Permits & s
addToPermits = do
  dip $ do
    unit
    some
  update

-- | Assert the parameter exists and deletes it from `Permits`
assertSentParam :: forall s. Permits & Permit & s :-> Permits & s
assertSentParam = do
  swap
  pair
  dup
  unpair
  mem @Permits
  if Holds
     then do
       dup
       car
       dip $ do
         cdr
         none
       update
     else do
       push $ mkMTextUnsafe "no permit"
       failWith

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

