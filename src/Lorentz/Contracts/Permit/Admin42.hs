{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Admin42 where

import Data.Maybe
import System.IO

import Lorentz
import Michelson.Text
import Util.IO
import Util.Named
import Tezos.Crypto.Hash

import qualified Lorentz.Contracts.Permit as Permit
import qualified Lorentz.Contracts.Permit.Type as Permit
-- import qualified Lorentz.Contracts.Revoke as Revoke

import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Base16 as Base16


permittableAdmin42Contract ::
     ContractCode (Permit.CheckSentParam Natural) (Permit.Storage Address)
permittableAdmin42Contract = do
  unpair
  Permit.unCheckSentParam
  unpair
  dip $ do
    dup
    push @Natural 42
    assertEq $ mkMTextUnsafe "not 42"
    pack
    dip $ do
      Permit.unStorage
      unpair
      dip $ do
        unpair
        swap
        dup
    swap
    dip $ pair
    pair
    Permit.toSentParam
  swap
  exec
  dip $ do
    swap
    pair
  pair
  Permit.toStorage
  nil
  pair

permitAdmin42Contract ::
     ContractCode (Permit.Parameter Natural) (Permit.Storage Address)
permitAdmin42Contract =
  Permit.permitWrapperContract
  permittableAdmin42Contract

-- | Print `permitAdmin42Contract`
--
-- @
--  printPermitAdmin42 (Just "contracts/permit_admin_42.tz") False
-- @
printPermitAdmin42 :: Maybe FilePath -> Bool -> IO ()
printPermitAdmin42 mOutput forceOneLine' =
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine' $
      (defaultContract permitAdmin42Contract) { cDisableInitialCast = True }

-- Tezos.Crypto.Orphans> A.printInitPermitAdmin42 (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
-- Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
printInitPermitAdmin42 :: Address -> IO ()
printInitPermitAdmin42 adminAddr =
  TL.putStrLn $
  printLorentzValue @(Permit.Storage Address) forceOneLine $
  Permit.mkStorage adminAddr
  where
    forceOneLine = True

printPermitAdmin42Bytes :: ChainId -> Address -> Natural -> IO ()
printPermitAdmin42Bytes chainId' contractAddr' counter' =
  print . ("0x" <>) . Base16.encode $
  blake2b $
  Permit.runPackWithChainId @Natural @(Permit.Parameter Natural) chainId' contractAddr' (#counter .! counter', (42 :: Natural))

printPermitAdmin42Param :: ChainId -> Address -> Natural -> PublicKey -> Signature -> IO ()
printPermitAdmin42Param chainId' contractAddr' counter' key' sig' =
  TL.putStrLn $
  printLorentzValue @Permit.SignedParams forceOneLine $
  -- print $ -- . ("0x" <>) . Base16.encode $
  Permit.SignedParams
    key'
    sig' $
    Permit.runPackWithChainId @Natural @(Permit.Parameter Natural) chainId' contractAddr' (#counter .! counter', (42 :: Natural))
  where
    forceOneLine = True


-- data SignedParams = SignedParams
--   { signer_key :: !PublicKey
--   , signature  :: !Signature
--   , param_hash :: !Blake2B



