{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Admin42 where

import Data.Either
import Data.Maybe
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Fail
import System.IO
import Text.Show

import Lorentz hiding (checkSignature)
import Michelson.Interpret
import Michelson.Test.Dummy
import Michelson.Text
import Tezos.Address
import Tezos.Crypto (checkSignature)
import Util.IO
import Util.Named

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

printPermitAdmin42Param :: ChainId -> Address -> Natural -> PublicKey -> Signature -> IO ()
printPermitAdmin42Param chainId' contractAddr' counter' key' sig'
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

    hashBytes' =
      Permit.runPackWithChainId @(Permit.Parameter Natural)
      chainId'
      contractAddr'
      (#counter .! counter')
      hash'

-- | Interpret Michelson code and generate corresponding bytestring.
runPermitAdmin42 :: ChainId -> Address -> Address -> Permit.SignedParams -> IO ()
runPermitAdmin42 chainId' selfAddress' adminAddress' xs = either
    (fail . fromString . show)
    (\(Identity (_, x) :& RNil) ->
      putStrLn "passed" *>
      print x *>
      TL.putStrLn (printLorentzValue True adminAddress') *>
      putStrLn "" *>
      return ()
    )
    $ interpretLorentzInstr
      env
      permitAdmin42Contract
      (Identity (Permit.PermitParam xs, Permit.mkStorage adminAddress') :& RNil)
  where
    env = dummyContractEnv { ceSelf = selfAddress', ceChainId = chainId' }

