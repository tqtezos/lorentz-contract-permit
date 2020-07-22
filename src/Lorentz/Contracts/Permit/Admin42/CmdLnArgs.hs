{-# OPTIONS -Wno-missing-export-lists -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Permit.Admin42.CmdLnArgs where

-- import Control.Applicative
-- import Text.Show (Show(..))
-- import Data.List
-- import Data.Eq
-- import Data.Either
-- import Data.Function (id)
-- import Data.Functor
-- import Data.Functor.Identity
-- import Prelude (FilePath, IO, Ord(..), print, putStrLn, Bifunctor(..))
-- import Data.Maybe
-- import Data.Typeable

-- import Lorentz hiding (chainId, checkSignature, get)
-- import Lorentz.Contracts.IsKey
-- import Lorentz.Run (interpretLorentzInstr)

-- import Michelson.Interpret
-- import Michelson.Parser
-- import Michelson.Test.Dummy
-- import Michelson.Typed.Annotation
-- import Michelson.Typed.EntryPoints hiding (parseEpAddress)
-- import Michelson.Typed.Haskell.Value
-- import Michelson.Typed.Instr
-- import Michelson.Typed.Scope
-- import Michelson.Typed.Sing
-- import Michelson.Typed.T
-- import Tezos.Crypto (checkSignature)
-- import Util.IO
-- import Util.Named
-- import qualified Michelson.TypeCheck.Types as TypeCheck

-- import qualified Options.Applicative as Opt
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy.IO as TL
-- import qualified Data.ByteString.Base16 as Base16
-- import Data.Constraint
-- import Data.Singletons

-- import Lorentz.Contracts.GenericMultisig.Parsers
-- import Michelson.Typed.Value.Missing ()
-- import Michelson.Typed.Sing.Missing
-- import Lorentz.Contracts.SomeContractParam
-- import Lorentz.Contracts.SomeContractStorage
-- import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G (parseTypeCheckValue)

-- import qualified Lorentz.Contracts.GenericMultisig as GenericMultisig
-- import qualified Lorentz.Contracts.GenericMultisig.Type as GenericMultisig

-- -- chainId :: ChainId

-- data CmdLnArgs
--   = Print
--       { outputPath :: Maybe FilePath
--       , oneline :: Bool
--       }
--   | Init
--       { admin :: Address
--       }
--   | Permit
--       { signer :: PublicKey
--       , signature :: Signature
--       , counter :: Natural
--       }

-- argParser :: Opt.Parser CmdLnArgs
-- argParser = Opt.hsubparser $ mconcat
--   [ printSubCmd
--   , initSubCmd
--   , permitSubCmd
--   , getCounterWrappedSubCmd
--   , changeKeysMultisigSubCmd
--   , runMultisigSubCmd
--   ]
--   where
--     mkCommandParser commandName parser desc =
--       Opt.command commandName $
--       Opt.info (Opt.helper <*> parser) $
--       Opt.progDesc desc

--     printSubCmd =
--       mkCommandParser "print"
--       (Print <$> outputOptions <*> onelineOption)
--       "Print the PermitAdmin42 contract in form of Michelson code"

--     initSubCmd =
--       mkCommandParser "init"
--       (Init <$>
--         parseAddress "admin"
--       )
--       "Print the intial storage for the Specialized Multisig contract"

--     permitSubCmd =
--       mkCommandParser "permit"
--       (Permit <$>
--         parsePublicKey "signer" <*>
--         parseSignature "signature" <*>
--         parseNatural "counter"
--       )
--       "Print the Permit parameter for the PermitAdmin42 contract"

-- infoMod :: Opt.InfoMod CmdLnArgs
-- infoMod = mconcat
--   [ Opt.fullDesc
--   , Opt.progDesc "PermitAdmin42 contract CLI interface"
--   ]

-- runCmdLnArgs :: CmdLnArgs -> IO ()
-- runCmdLnArgs = \case
--   Print223 mOutput forceOneLine' ->
--     maybe TL.putStrLn writeFileUtf8 mOutput $
--     printLorentzContract forceOneLine' $
--     GenericMultisig.generigMultisigContract223
--   PrintSpecialized (SomeSing (st :: Sing t)) mOutput forceOneLine' ->
--     withDict (singIT st) $
--     withDict (singTypeableT st) $
--     assertOpAbsense @t $
--     assertBigMapAbsense @t $
--     assertNestedBigMapsAbsense @t $
--     maybe TL.putStrLn writeFileUtf8 mOutput $
--     printLorentzContract forceOneLine' $
--     GenericMultisig.specializedMultisigContract @(Value t) @PublicKey
--   PrintWrapped wrappedContract mOutput oneline ->
--     case wrappedContract of
--       TypeCheck.SomeContract wrappedContractFC ->
--         case wrappedContractFC of
--           FullContract wrappedContractCode (_ :: ParamNotes cp) (_ :: Notes st) ->
--             assertBigMapAbsense @cp $
--             maybe TL.putStrLn writeFileUtf8 mOutput $
--             printLorentzContract oneline $
--             GenericMultisig.genericMultisigContractWrapper @(Value cp) @(Value st) @PublicKey
--             (I wrappedContractCode)
--   Init223 {..} ->
--     if threshold > genericLength signerKeyPairs
--        then error "threshold is greater than the number of signer keys"
--        else TL.putStrLn $
--          printLorentzValue @(GenericMultisig.Storage (PublicKey, PublicKey)) forceOneLine $
--          ( #counter .! GenericMultisig.initialMultisigCounter
--          , ( #threshold .! threshold
--            , #keys      .! signerKeyPairs
--            )
--          )
--   InitSpecialized {..} ->
--     if threshold > genericLength signerKeys
--        then error "threshold is greater than the number of signer keys"
--        else TL.putStrLn $
--          printLorentzValue @(GenericMultisig.Storage PublicKey) forceOneLine $
--          ( #counter .! GenericMultisig.initialMultisigCounter
--          , ( #threshold .! threshold
--            , #keys      .! signerKeys
--            )
--          )
--   InitWrapped (SomeContractStorage (initialWrappedStorage :: Value st)) threshold' signerKeys' ->
--     TL.putStrLn $
--     withDict (singTypeableT (sing @st)) $
--     printLorentzValue @(Value st, GenericMultisig.Storage PublicKey) forceOneLine $
--     ( initialWrappedStorage
--     , ( #counter .! GenericMultisig.initialMultisigCounter
--       , ( #threshold .! threshold'
--         , #keys      .! signerKeys'
--         )
--       )
--     )
--   GetCounterSpecialized {..} ->
--     let parsedStorage = parseNoEnv
--           (G.parseTypeCheckValue @(ToT (GenericMultisig.Storage PublicKey)))
--           "specialized-multisig"
--           storageText
--      in let (storedCounter, (_threshold, storedSignerKeys)) =
--               either
--                 (error . T.pack . show)
--                 (fromVal @(GenericMultisig.Storage PublicKey))
--                 parsedStorage
--          in if arg #keys storedSignerKeys == signerKeys
--                then print storedCounter
--                else do
--                  putStrLn @Text "Stored signer keys:"
--                  print signerKeys
--                  error "Stored signer keys do not match provided signer keys"
--   GetCounterWrapped (SomeSing (st :: Sing t)) storageText' signerKeys' ->
--     withDict (singIT st) $
--     withDict (singTypeableT st) $
--     let parsedStorage = parseNoEnv
--           (G.parseTypeCheckValue @(ToT (Value t, GenericMultisig.Storage PublicKey)))
--           "specialized-multisig"
--           storageText'
--      in let (_, (storedCounter, (_threshold, storedSignerKeys))) =
--               either
--                 (error . T.pack . show)
--                 (fromVal @(Value t, GenericMultisig.Storage PublicKey))
--                 parsedStorage
--          in if arg #keys storedSignerKeys == signerKeys'
--                then print storedCounter
--                else do
--                  putStrLn @Text "Stored signer keys:"
--                  print signerKeys'
--                  error "Stored signer keys do not match provided signer keys"
--   ChangeKeysMultisig {..} ->
--     let changeKeysParam = (counter, GenericMultisig.ChangeKeys @PublicKey @((), ContractRef ()) (#threshold .! threshold, #keys .! newSignerKeys))
--         bytes = packer @PublicKey @((), ContractRef ()) @(GenericMultisig.ChangeKeyParams PublicKey) chainId multisigContract changeKeysParam
--     in
--     if threshold > genericLength newSignerKeys
--        then error "threshold is greater than the number of signer keys"
--        else
--        case signatures of
--          Nothing -> print . ("0x" <>) . Base16.encode $ bytes
--          -- . lPackValue . asPackType @((), ContractRef ()) $ (toAddress targetContract, changeKeysParam)
--          Just someSignatures ->
--             if checkSignaturesValid bytes $ zip signerKeys someSignatures
--                then
--                  TL.putStrLn $
--                  printLorentzValue @(GenericMultisig.MainParams PublicKey ((), ContractRef ())) forceOneLine $
--                  asParameterType $
--                  (bimap (#counter .!) (#action .!) changeKeysParam, #sigs .! someSignatures)
--                else error "invalid signature(s) provided"
--   RunMultisig {..} ->
--     case contractParameter of
--       SomeContractParam (param :: Value cp) _ (Dict, Dict) ->
--         assertNestedBigMapsAbsense @cp $
--         let runParam =
--               ( counter
--               , GenericMultisig.Operation
--                   @PublicKey
--                   @(Value cp, ContractRef (Value cp))
--                   (param, unsafeRootContractRef @cp targetContract))
--             bytes = packer
--                    @PublicKey
--                    @(Value cp, ContractRef (Value cp))
--                    @(GenericMultisig.MainParams PublicKey (Value cp, ContractRef (Value cp)))
--                    chainId multisigContract runParam
--          in case signatures of
--               Nothing -> print . ("0x" <>) . Base16.encode $ bytes
--               Just someSignatures ->
--                 if checkSignaturesValid bytes $ zip signerKeys someSignatures
--                    then
--                      TL.putStrLn $
--                      printLorentzValue @(GenericMultisig.MainParams PublicKey (Value cp, ContractRef (Value cp))) forceOneLine $
--                      asParameterType $
--                      (bimap (#counter .!) (#action .!) runParam, #sigs .! someSignatures)
--                    else error "invalid signature(s) provided"
--   where
--     forceOneLine = True

--     asParameterType :: forall cp. GenericMultisig.MainParams PublicKey cp -> GenericMultisig.MainParams PublicKey cp
--     asParameterType = id

-- checkSignaturesValid :: ByteString -> [(PublicKey, Maybe Signature)] -> Bool
-- checkSignaturesValid = all . checkSignatureValid

-- checkSignatureValid :: ByteString -> (PublicKey, Maybe Signature) -> Bool
-- checkSignatureValid _ (_, Nothing) = True
-- checkSignatureValid a (pubKey, Just sig) = checkSignature pubKey sig a