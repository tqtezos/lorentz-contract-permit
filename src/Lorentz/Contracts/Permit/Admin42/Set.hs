{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Admin42.Set where

import Data.Bool
import Data.Function
import Data.Maybe
import System.IO

import Lorentz -- hiding (checkSignature)
import Michelson.Text
import Util.IO
import Util.Named

import Tezos.Crypto.Orphans ()
import qualified Lorentz.Contracts.Permit.Set as Permit
import qualified Lorentz.Contracts.Permit.Set.Type as Permit

import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Base16 as Base16

permittableAdmin42SetContract :: forall t s.
  Natural & (t, Permit.Storage Address) & s :-> ([Operation], Permit.Storage Address) & s
permittableAdmin42SetContract = do
  dup
  push @Natural 42
  assertEq $ mkMTextUnsafe "not 42"
  pack
  Permit.safeBlake2B
  dip $ do
    cdr
    Permit.unStorage
    dup
    car
    dip $ do
      cdr
      dup
      cdr
  swap
  dip $ do
    pair
    Permit.toPermit
  Permit.assertSentParam
  pair
  Permit.toStorage
  nil
  pair

permitAdmin42SetContract ::
     ContractCode (Permit.Parameter Natural) (Permit.Storage Address)
permitAdmin42SetContract =
  Permit.permitWrapperContract
  permittableAdmin42SetContract

-- | Print `permitAdmin42Contract`
--
-- @
--  printPermitAdmin42 (Just "contracts/permit_admin_42.tz") False
-- @
printPermitAdmin42Set :: Maybe FilePath -> Bool -> IO ()
printPermitAdmin42Set mOutput forceOneLine' =
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine' $
      (defaultContract permitAdmin42SetContract)
        { cDisableInitialCast = True }

-- Tezos.Crypto.Orphans> A.printInitPermitAdmin42 (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
-- Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
printInitPermitAdmin42Set :: Address -> IO ()
printInitPermitAdmin42Set adminAddr =
  TL.putStrLn $
  printLorentzValue @(Permit.Storage Address) forceOneLine $
  Permit.mkStorage adminAddr
  where
    forceOneLine = True

instance ParameterHasEntryPoints (Permit.Parameter Natural) where
  type ParameterEntryPointsDerivation (Permit.Parameter Natural) = EpdPlain

printPermitAdmin42SetBytes :: ChainId -> Address -> Natural -> IO ()
printPermitAdmin42SetBytes chainId' contractAddr' counter' =
  print . ("0x" <>) . Base16.encode $
  Permit.runPackWithChainId @(Permit.Parameter Natural)
    chainId'
    contractAddr'
    (#counter .! counter')
    hash'
  where
    bytes' :: ByteString
    bytes' = lPackValue (42 :: Natural)

    hash' :: Permit.Blake2B
    hash' = Permit.mkBlake2B bytes'

printPermitAdmin42SetParam :: PublicKey -> Signature -> IO () -- ChainId -> Address -> Natural ->
printPermitAdmin42SetParam key' sig' -- _chainId' _contractAddr' _counter'
  | True = -- checkSignature key' sig' hashBytes'
      let signedParams' = Permit.SignedParams key' sig' hash' in
      -- runPermitAdmin42 chainId' contractAddr' (mkKeyAddress key') signedParams' *>
      TL.putStrLn (
        printLorentzValue @Permit.SignedParams forceOneLine signedParams'
        )
  -- | True = error "missigned"
  where
    forceOneLine = True

    bytes' :: ByteString
    bytes' = lPackValue (42 :: Natural)

    hash' :: Permit.Blake2B
    hash' = Permit.mkBlake2B bytes'

