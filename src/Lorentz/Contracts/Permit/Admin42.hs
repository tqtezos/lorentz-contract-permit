{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Permit.Admin42 where

import Data.Bool
import Data.Function
import Data.Maybe
import System.IO

-- import Tezos.Crypto (checkSignature)
import Lorentz -- hiding (checkSignature)
import Michelson.Text
import Util.IO
import Util.Named

import Tezos.Crypto.Orphans ()
import Lorentz.Contracts.Admin42
import qualified Lorentz.Contracts.Permit.Paired as Permit
import qualified Lorentz.Contracts.Permit.Paired.Type as Permit
-- import qualified Lorentz.Contracts.Revoke as Revoke

import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Base16 as Base16


permittableAdmin42Contract :: forall t s.
  Natural & (t, Permit.Storage Address) & s :-> ([Operation], Permit.Storage Address) & s
     -- ContractCode Natural (Permit.Storage Address)
permittableAdmin42Contract = do
  -- unpair
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

permitAdmin42Contract ::
     ContractCode (Permit.Parameter Natural) (Permit.Storage Address)
permitAdmin42Contract =
  Permit.permitWrapperContract
  permittableAdmin42Contract

printAdmin42 :: Maybe FilePath -> Bool -> IO ()
printAdmin42 mOutput forceOneLine' =
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine' $
      (defaultContract admin42Contract) { cDisableInitialCast = True }

-- | Print `permitAdmin42Contract`
--
-- @
--  printPermitAdmin42 (Just "contracts/permit_admin_42.tz") False
-- @
printPermitAdmin42 :: Maybe FilePath -> Bool -> IO ()
printPermitAdmin42 mOutput forceOneLine' =
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine' $
      (defaultContract permitAdmin42Contract)
        { cDisableInitialCast = True }

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

printPermitAdmin42Param :: PublicKey -> Signature -> IO () -- ChainId -> Address -> Natural ->
printPermitAdmin42Param key' sig' -- _chainId' _contractAddr' _counter'
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

    -- hashBytes' =
    --   Permit.runPackWithChainId @(Permit.Parameter Natural)
    --   chainId'
    --   contractAddr'
    --   (#counter .! counter')
    --   hash'

-- -- | Interpret Michelson code and generate corresponding bytestring.
-- runPermitAdmin42 :: ChainId -> Address -> Address -> Permit.SignedParams -> IO ()
-- runPermitAdmin42 chainId' selfAddress' adminAddress' xs = either
--     (fail . fromString . show)
--     (\(Identity (_, x) :& RNil) ->
--       putStrLn "passed" *>
--       print x *>
--       TL.putStrLn (printLorentzValue True adminAddress') *>
--       putStrLn "" *>
--       return ()
--     )
--     $ interpretLorentzInstr
--       env
--       permitAdmin42Contract
--       (Identity (Permit.Permit xs, Permit.mkStorage adminAddress') :& RNil)
--   where
--     env = dummyContractEnv { ceSelf = selfAddress', ceChainId = chainId' }





-- -- | Interpret Michelson code and generate corresponding bytestring.
-- runPermitAdmin42 :: Address -> ChainId -> Text -> PublicKey -> IO ()
-- runPermitAdmin42 selfAddress' chainId' storageTxt pubKey' =
--   withAdmin42Storage storageTxt $ \storage' ->
--     either
--       (\x -> case x of
--                MichelsonFailedWith (xs :: Value t) ->
--                  case sing @t of
--                    STPair STString (STPair STBytes (STPair (STPair STChainId STAddress) (STPair STNat STBytes))) ->
--                      case fromVal @(MText, (ByteString, ((ChainId, Address), ("counter" :! Natural, Permit.Blake2B)))) xs of
--                        (missigned', (bytes'', ys)) ->
--                          bool
--                             (error . fromString . ("runPermitAdmin42: unexpected failure: " <>) $ show x)
--                             (TL.putStrLn (printLorentzValue False ys) *>
--                              print ("0x" <> Base16.encode bytes''))
--                             (missigned' == mkMTextUnsafe "missigned")
--                    _ -> error . fromString . ("runPermitAdmin42: unexpected failure: " <>) $ show x
--                _ -> error . fromString $ show x
--       )
--       (\(Identity (_, x) :& RNil) -> print x)
--
--   -- either
--   --   (fail . fromString . show)
--   --   (\(Identity (_, x) :& RNil) -> _ x)
--     $
--     interpretLorentzInstr env permitAdmin42Contract (Identity (Permit.Permit $ Permit.SignedParams pubKey' dummySignature hash', storage') :& RNil)
--   where
--     env = dummyContractEnv { ceSelf = selfAddress', ceChainId = chainId' }
--
--     bytes' :: ByteString
--     bytes' = lPackValue (42 :: Natural)
--
--     dummySignature = read "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM"
--
--     hash' :: Permit.Blake2B
--     hash' = Permit.mkBlake2B bytes'

