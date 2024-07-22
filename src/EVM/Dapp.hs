{-# Language DataKinds #-}

module EVM.Dapp where

import EVM.ABI
import EVM.Concrete
import EVM.Solidity
import EVM.Types

import Control.Arrow ((>>>), second)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (find, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text, isPrefixOf, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector qualified as V
import Optics.Core
import Witch (unsafeInto)

data DappInfo = DappInfo
  { root       :: FilePath
  , solcByName :: Map Text SolcContract
  , solcByHash :: Map W256 (CodeType, SolcContract)
  , solcByCode :: [(Code, SolcContract)] -- for contracts with `immutable` vars.
  , sources    :: SourceCache
  , unitTests  :: [(Text, [Sig])]
  , abiMap     :: Map FunctionSelector Method
  , eventMap   :: Map W256 Event
  , errorMap   :: Map W256 SolError
  , astIdMap   :: Map Int Value
  , astSrcMap  :: SrcMap -> Maybe Value
  }

-- | bytecode modulo immutables, to identify contracts
data Code = Code
  { raw :: ByteString
  , immutableLocations :: [Reference]
  }
  deriving Show

data DappContext = DappContext
  { info :: DappInfo
  , contracts :: Map (Expr EAddr) Contract
  , labels :: Map Addr Text
  }

dappInfo :: FilePath -> BuildOutput -> DappInfo
dappInfo root (BuildOutput (Contracts cs) sources) =
  let
    solcs = Map.elems cs
    astIds = astIdMap $ snd <$> Map.toList sources.asts
    immutables = filter ((/=) mempty . (.immutableReferences)) solcs

  in DappInfo
    { root = root
    , unitTests = findAllUnitTests solcs
    , sources = sources
    , solcByName = cs
    , solcByHash =
        let
          f g k = Map.fromList [(g x, (k, x)) | x <- solcs]
        in
          mappend
           (f (.runtimeCodehash)  Runtime)
           (f (.creationCodehash) Creation)
      -- contracts with immutable locations can't be id by hash
    , solcByCode =
      [(Code x.runtimeCode (concat $ Map.elems x.immutableReferences), x) | x <- immutables]
      -- Sum up the ABI maps from all the contracts.
    , abiMap   = mconcat (map (.abiMap) solcs)
    , eventMap = mconcat (map (.eventMap) solcs)
    , errorMap = mconcat (map (.errorMap) solcs)

    , astIdMap  = astIds
    , astSrcMap = astSrcMap astIds
    }

emptyDapp :: DappInfo
emptyDapp = dappInfo "" mempty

-- Dapp unit tests are detected by searching within abi methods
-- that begin with "check" or "prove", that are in a contract with
-- the "IS_TEST()" abi marker, for a given regular expression.
--
-- The regex is matched on the full test method name, including path
-- and contract, i.e. "path/to/file.sol:TestContract.test_name()".

unitTestMarkerAbi :: FunctionSelector
unitTestMarkerAbi = abiKeccak (encodeUtf8 "IS_TEST()")

findAllUnitTests :: [SolcContract] -> [(Text, [Sig])]
findAllUnitTests = findUnitTests ".*:.*\\.(check|prove).*"

mkSig :: Method -> Maybe Sig
mkSig method
  | "prove" `isPrefixOf` testname = Just (Sig testname argtypes)
  | "check" `isPrefixOf` testname = Just (Sig testname argtypes)
  | otherwise = Nothing
  where
    testname = method.methodSignature
    argtypes = snd <$> method.inputs

findUnitTests :: Text -> ([SolcContract] -> [(Text, [Sig])])
findUnitTests match =
  concatMap $ \c ->
    case Map.lookup unitTestMarkerAbi c.abiMap of
      Nothing -> []
      Just _  ->
        let testNames = unitTestMethodsFiltered (regexMatches match) c
        in [(c.contractName, testNames) | not (BS.null c.runtimeCode) && not (null testNames)]

unitTestMethodsFiltered :: (Text -> Bool) -> (SolcContract -> [Sig])
unitTestMethodsFiltered matcher c =
  let testName (Sig n _) = c.contractName <> "." <> n
  in filter (matcher . testName) (unitTestMethods c)

unitTestMethods :: SolcContract -> [Sig]
unitTestMethods =
  (.abiMap)
  >>> Map.elems
  >>> mapMaybe mkSig

traceSrcMap :: DappInfo -> Trace -> Maybe SrcMap
traceSrcMap dapp trace = srcMap dapp trace.contract trace.opIx

srcMap :: DappInfo -> Contract -> Int -> Maybe SrcMap
srcMap dapp contr opIndex = do
  sol <- findSrc contr dapp
  case contr.code of
    UnknownCode _ -> Nothing
    InitCode _ _ ->
     Seq.lookup opIndex sol.creationSrcmap
    RuntimeCode _ ->
      Seq.lookup opIndex sol.runtimeSrcmap

findSrc :: Contract -> DappInfo -> Maybe SolcContract
findSrc c dapp = do
  hash <- maybeLitWord c.codehash
  case Map.lookup hash dapp.solcByHash of
    Just (_, v) -> Just v
    Nothing -> lookupCode c.code dapp


lookupCode :: ContractCode -> DappInfo -> Maybe SolcContract
lookupCode (UnknownCode _) _ = Nothing
lookupCode (InitCode c _) a =
  snd <$> Map.lookup (keccak' (stripBytecodeMetadata c)) a.solcByHash
lookupCode (RuntimeCode (ConcreteRuntimeCode c)) a =
  case snd <$> Map.lookup (keccak' (stripBytecodeMetadata c)) a.solcByHash of
    Just x -> pure x
    Nothing -> snd <$> find (compareCode c . fst) a.solcByCode
lookupCode (RuntimeCode (SymbolicRuntimeCode c)) a = let
    code = BS.pack $ mapMaybe maybeLitByte $ V.toList c
  in case snd <$> Map.lookup (keccak' (stripBytecodeMetadata code)) a.solcByHash of
    Just x -> pure x
    Nothing -> snd <$> find (compareCode code . fst) a.solcByCode

compareCode :: ByteString -> Code -> Bool
compareCode raw (Code template locs) =
  let holes' = sort [(start, len) | (Reference start len) <- locs]
      insert loc len' bs = writeMemory (BS.replicate len' 0) (unsafeInto len') 0 (unsafeInto loc) bs
      refined = foldr (\(start, len) acc -> insert start len acc) raw holes'
  in BS.length raw == BS.length template && template == refined

showTraceLocation :: DappInfo -> Trace -> Either Text Text
showTraceLocation dapp trace =
  case traceSrcMap dapp trace of
    Nothing -> Left "<no source map>"
    Just sm ->
      case srcMapCodePos dapp.sources sm of
        Nothing -> Left "<source not found>"
        Just (fileName, lineIx) ->
          Right (pack fileName <> ":" <> pack (show lineIx))

srcMapCodePos :: SourceCache -> SrcMap -> Maybe (FilePath, Int)
srcMapCodePos cache sm =
  fmap (second f) $ cache.files ^? ix sm.file
  where
    f v = BS.count 0xa (BS.take sm.offset v) + 1

srcMapCode :: SourceCache -> SrcMap -> Maybe ByteString
srcMapCode cache sm =
  fmap f $ cache.files ^? ix sm.file
  where
    f (_, v) = BS.take (min 80 sm.length) (BS.drop sm.offset v)
