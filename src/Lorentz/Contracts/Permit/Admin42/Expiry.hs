{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Admin42.Expiry where

import GHC.Base (join)
import Data.Bool
import Data.Function
import Data.Maybe
import System.IO

import Lorentz -- hiding (checkSignature)
import Michelson.Text
import Util.IO
import Util.Named

import Tezos.Crypto.Orphans ()
import qualified Lorentz.Contracts.Permit.Expiry as Expiry
import qualified Lorentz.Contracts.Permit.Expiry.Type as Expiry

-- import qualified Lorentz.Contracts.Permit as Permit
import qualified Lorentz.Contracts.Permit.Type as Permit

import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Base16 as Base16

instance ParameterHasEntryPoints (Expiry.Parameter Natural) where
  type ParameterEntryPointsDerivation (Expiry.Parameter Natural) = EpdPlain

instance ParameterHasEntryPoints (Permit.Parameter (Expiry.Parameter Natural)) where
  type ParameterEntryPointsDerivation (Permit.Parameter (Expiry.Parameter Natural)) = EpdRecursive

permitAdmin42ExpiryContract ::
     ContractCode (Permit.Parameter (Expiry.Parameter Natural)) (Expiry.Storage Address)
permitAdmin42ExpiryContract =
  Expiry.permitExpiryWrapperContract
  permittableAdmin42ExpiryContract

permittableAdmin42ExpiryContract :: ()
  => '[Natural, (Permit.Parameter (Expiry.Parameter Natural), Expiry.Storage Address)]
  :-> '[([Operation], Expiry.Storage Address)]
permittableAdmin42ExpiryContract = do
  push @Natural 42
  assertEq $ UnspecifiedError -- mkMTextUnsafe "not 42"
  dup
  dip car
  cdr
  dup
  Expiry.unStorage
  cdr
  cdr
  dup
  sender
  ifEq
    (do
      drop
      swap
      drop
    )
    (do
      dig @2
      pack
      Permit.safeBlake2B
      pair
      Permit.toPermit
      swap
      stackType @('[Expiry.Storage Address, Permit.Permit])
      Expiry.unStorage
      dup
      car
      dip $ do
        stackType @('[(Expiry.Expiry, (Permit.Storage Expiry.Permits Address, Address)), Permit.Permit])
        cdr
        unpair
        Permit.unStorage
        dup
        cdr
        dip $ do
          stackType @('[(Expiry.Permits, ("counter" :! Natural, Address)), Address, Permit.Permit])
          car
          dup
          dip $ do
            stackType @('[Expiry.Permits, Address, Permit.Permit])
            dig @2
            dup
            dip $ do
              stackType @('[Permit.Permit, Expiry.Permits, Address])
              Permit.permitSigner
              get
              assertSome $ UnspecifiedError -- mkMTextUnsafe "no permit"
              Expiry.unPermitExpiry
              dup
              cdr
            dup
            dip $ do
              stackType @('[Permit.Permit, Map Permit.Blake2B Expiry.CreatedAt, (Maybe Expiry.Expiry, Map Permit.Blake2B Expiry.CreatedAt), Address])
              Permit.permitParamHash
              dup
              dip $ do
                stackType @('[Permit.Blake2B, Map Permit.Blake2B Expiry.CreatedAt, (Maybe Expiry.Expiry, Map Permit.Blake2B Expiry.CreatedAt), Address])
                get
                assertSome $ UnspecifiedError -- mkMTextUnsafe "no permit"
                Expiry.runCreatedAt
                swap
                dup
                cdr
                dip car
                none @Expiry.CreatedAt
              update
              swap
              pair
              Expiry.toPermitExpiry
              some
            Permit.permitSigner
          dug @2
          update
        swap
        pair
        swap
      dup
      dip $ do
        stackType @('[Expiry.Expiry, Maybe Timestamp, (Expiry.Permits, ("counter" :! Natural, Address)), Address])
        Expiry.assertCreatedAtExpiry
        Permit.toStorage
        pair
      pair
      Expiry.toStorage
    )
  nil
  pair

-- | Print `permitAdmin42ExpiryContract`
--
-- @
--  printPermitAdmin42Expiry (Just "contracts/permit_admin_42_expiry.tz") False
-- @
printPermitAdmin42Expiry :: Maybe FilePath -> Bool -> IO ()
printPermitAdmin42Expiry mOutput forceOneLine' =
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine' $
      (defaultContract permitAdmin42ExpiryContract)
        { cDisableInitialCast = True }

-- Text.Read Tezos.Crypto.Orphans> printInitPermitAdmin42Expiry 300 (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
-- Pair 300 (Pair (Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")) "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
printInitPermitAdmin42Expiry :: Expiry.SafeExpiry -> Address -> IO ()
printInitPermitAdmin42Expiry defaultExpiry' adminAddr =
  TL.putStrLn $
  printLorentzValue @(Expiry.Storage Address) forceOneLine $
  join (Expiry.mkStorage defaultExpiry') adminAddr
  where
    forceOneLine = True


