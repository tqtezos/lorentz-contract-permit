{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Expiry where

import Lorentz
import Michelson.Text

import Lorentz.Contracts.Permit.Expiry.Type
import qualified Lorentz.Contracts.Permit as Permit
import qualified Lorentz.Contracts.Permit.Type as Permit

import Text.Show

-- | Given a `Contract` that requires a "`sender` check"-like
-- operation, this adds functionality so that a user can pre-approve parameters,
-- allowing others to submit those parameters on their behalf
permitExpiryWrapperContract ::
     forall cp st.
     ( HasTypeAnn cp
     , NiceParameterFull cp
     , ParameterHasEntryPoints (Parameter cp)
     , KnownValue st
     )
  => cp & (Permit.Parameter (Parameter cp), Storage st) & '[] :-> ([Operation], Storage st) & '[]
  -> ContractCode (Permit.Parameter (Parameter cp)) (Storage st)
permitExpiryWrapperContract targetContract = do
  dup
  car
  caseT @(Permit.Parameter (Parameter cp))
    ( #cPermit /-> do
        permitParamStorageContains @cp @_ @st
        nil
        pair
    , #cWrapped /-> do
        caseT @(Parameter cp)
          ( #cSetExpiry /-> do
              setExpiry
              nil
              pair
          , #cDefaultExpiry /-> do
              dip cdr
              updateDefaultExpiry
              nil
              pair
          , #cWrapped /-> targetContract
          )

    )

checkPermitStorageContains ::
     forall cp t st s.
     ( HasTypeAnn cp
     , NiceParameterFull cp
     , ParameterHasEntryPoints (Parameter cp)
     , StorageContains st '[ "counter" := ("counter" :! Natural)]
     )
  => SignedParams & (t, st) & s :-> Permit & st & s
checkPermitStorageContains = do
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
        dip Permit.publicKeyToAddress
        pair
        swap
      swap
      cdr
      stGetField #counter
      dip swap
      Permit.packWithChainId @(Parameter cp)
  Permit.assertSignature
  swap
  toPermit

permitParamStorageContains ::
     forall cp t st s.
     ( HasTypeAnn cp
     , NiceParameterFull cp
     , ParameterHasEntryPoints (Parameter cp)
     , IsoValue st
     )
  => SignedParams & (t, Storage st) & s :-> Storage st & s
permitParamStorageContains = do
  checkPermitStorageContains @cp @t @(Storage st)
  addPermit -- StorageContains

setExpiry :: forall cp st. '[ExpiryParams, (Permit.Parameter (Parameter cp), Storage st)] :-> '[Storage st]
setExpiry = do
  unExpiryParams
  dup
  car
  dip $ do
    stackType @('[(Maybe Blake2B, (Address, SafeExpiry)), (Permit.Parameter (Parameter cp), Storage st)])
    dup
    cdr
    dup
    dip $ do
      stackType @('[(Address, SafeExpiry), (Maybe Blake2B, (Address, SafeExpiry)), (Permit.Parameter (Parameter cp), Storage st)])
      car
      dup
      sender
      ifEq
        (do
          drop
          drop
        )
        (do
          stackType @('[Address, (Maybe Blake2B, (Address, SafeExpiry)), (Permit.Parameter (Parameter cp), Storage st)])
          swap
          pack
          safeBlake2B
          pair
          toPermit
          swap
          unpair
          dip $ do
            stackType @('[Storage st, Permit])
            unStorage
            dup
            car
            dip $ do
              stackType @('[(Expiry, (Permit.Storage Permits st, Address)), Permit])
              cdr
              unpair
              Permit.unStorage
              dup
              cdr
              dip $ do
                stackType @('[(Permits, ("counter" :! Natural, st)), Address, Permit])
                car
                dup
                dip $ do
                  stackType @('[Permits, Address, Permit])
                  dig @2
                  dup
                  dip $ do
                    stackType @('[Permit, Permits, Address])
                    Permit.permitSigner
                    get
                    assertSome $ UnspecifiedError -- mkMTextUnsafe "no permit"
                    unPermitExpiry
                    dup
                    cdr
                  dup
                  dip $ do
                    stackType @('[Permit, Map Blake2B CreatedAt, (Maybe Expiry, Map Blake2B CreatedAt), Address])
                    Permit.permitParamHash
                    dup
                    dip $ do
                      stackType @('[Blake2B, Map Blake2B CreatedAt, (Maybe Expiry, Map Blake2B CreatedAt), Address])
                      get
                      assertSome $ UnspecifiedError -- mkMTextUnsafe "no permit"
                      runCreatedAt
                      swap
                      dup
                      cdr
                      dip car
                      none @CreatedAt
                    update
                    swap
                    pair
                    toPermitExpiry
                    some
                  Permit.permitSigner
                dug @2
                update
              swap
              pair
              swap
            dup
            dip $ do
              stackType @('[Expiry, Maybe Timestamp, (Permits, ("counter" :! Natural, st)), Address])
              assertCreatedAtExpiry
              Permit.toStorage
              pair
            pair
            toStorage
          pair
        )
    dip $ do
      stackType @('[(Permit.Parameter (Parameter cp), Storage st)])
      cdr
  ifNone
    updateUserExpiry
    updatePermitExpiry

