{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Set.Type
  ( module Lorentz.Contracts.Permit.Set.Type
  , Permit(..)
  , SignedParams(..)
  , Parameter(..)
  , DummyStorage(..)
  , Blake2B
  , mkBlake2B
  , safeBlake2B
  , toPermit
  , unPermit
  , unStorage
  , toStorage
  , mkStorage
  , incrementCounter
  , unSignedParams
  ) where

import Lorentz

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
  , unPermit
  , toStorage
  , unSignedParams
  , unStorage
  )
import qualified Lorentz.Contracts.Permit.Type as Permit

type PermitSet = Set Blake2B

type Permits = BigMap Address PermitSet

getPermitSet :: Address & Permits & s :-> PermitSet & s
getPermitSet = do
  get
  ifNone emptySet nop

type Storage = Permit.Storage Permits

-- | Assert the parameter exists and deletes it from `Permits`
assertSentParam :: forall s. Permits & Permit & s :-> Permits & s
assertSentParam = do
  swap
  pair
  dup
  unpair
  unPermit
  dup
  cdr
  dip swap
  get @Permits
  if IsNone
     then failWith
     else do
       swap
       dip dup
       car
       mem @PermitSet
       if Holds
          then do
            dip $ do
              unpair
              unPermit
              dup
              car
            swap
            dip $ push False
            update @PermitSet
            some
            swap
            cdr
            update @Permits
          else failWith

-- | `assertSentParam`, using `StorageContains`
assertSentParamStorageContains :: forall s store. StorageContains store '["permits" := Address ~> PermitSet] => Permit & store & s :-> store & s
assertSentParamStorageContains = do
  pair
  dup
  unpair
  unPermit
  dup
  cdr
  dip swap
  stGet #permits
  if IsNone
     then failWith
     else do
       swap
       dip dup
       car
       mem @PermitSet
       if Holds
          then do
            dip $ do
              unpair
              unPermit
              dup
              car
            swap
            dip $ push False
            update @PermitSet
            some
            swap
            cdr
            stUpdate #permits
          else failWith

addPermit :: forall st s. Permit & Storage st & s :-> Storage st & s
addPermit = do
  unPermit
  dup
  car
  stackType @(Blake2B & ((Blake2B, Address) & (Storage st & s)))
  dip $ do
    dip $ do
      unStorage
      dup
      cdr
      incrementCounter
      swap
      car
      dup
    cdr
    dup
    dip swap
    getPermitSet
    push True
  update @PermitSet
  some
  swap
  update @Permits
  pair
  toStorage


