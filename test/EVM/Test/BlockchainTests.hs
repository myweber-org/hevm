module EVM.Test.BlockchainTests where

import EVM (initialContract, makeVm)
import EVM.Concrete qualified as EVM
import EVM.FeeSchedule (feeSchedule)
import EVM.Fetch qualified
import EVM.Format (hexText)
import EVM.Stepper qualified
import EVM.Transaction
import EVM.Types hiding (Block, Case, Env)
import EVM.Test.Tracing (interpretWithTrace, VMTrace, compareTraces, EVMToolTraceOutput(..))

import Optics.Core
import Control.Arrow ((***), (&&&))
import Control.Monad
import Control.Monad.ST (RealWorld, stToIO)
import Control.Monad.State.Strict
import Control.Monad.IO.Unlift
import EVM.Effects
import Data.Aeson ((.:), (.:?), FromJSON (..))
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (isInfixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isNothing, isJust)
import Data.Word (Word64)
import System.Environment (lookupEnv, getEnv)
import System.FilePath.Find qualified as Find
import System.FilePath.Posix (makeRelative, (</>))
import Witch (into, unsafeInto)
import Witherable (Filterable, catMaybes)

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

data Which = Pre | Post

data Block = Block
  { coinbase    :: Addr
  , difficulty  :: W256
  , mixHash     :: W256
  , gasLimit    :: Word64
  , baseFee     :: W256
  , number      :: W256
  , timestamp   :: W256
  , txs         :: [Transaction]
  } deriving Show

data Case = Case
  { vmOpts      :: VMOpts Concrete
  , checkContracts  :: Map Addr Contract
  , testExpectation :: Map Addr Contract
  } deriving Show

data BlockchainCase = BlockchainCase
  { blocks  :: [Block]
  , pre     :: Map Addr Contract
  , post    :: Map Addr Contract
  , network :: String
  } deriving Show


testEnv :: Env
testEnv = Env { config = defaultConfig }

main :: IO ()
main = do
  tests <- runEnv testEnv prepareTests
  defaultMain tests

prepareTests :: App m => m TestTree
prepareTests = do
  repo <- liftIO $ getEnv "HEVM_ETHEREUM_TESTS_REPO"
  let testsDir = "BlockchainTests/GeneralStateTests"
  let dir = repo </> testsDir
  jsonFiles <- liftIO $ Find.find Find.always (Find.extension Find.==? ".json") dir
  liftIO $ putStrLn "Loading and parsing json files from ethereum-tests..."
  isCI <- liftIO $ isJust <$> lookupEnv "CI"
  let problematicTests = if isCI then commonProblematicTests <> ciProblematicTests else commonProblematicTests
  let ignoredFiles = if isCI then ciIgnoredFiles else []
  groups <- mapM (\f -> testGroup (makeRelative repo f) <$> (if any (`isInfixOf` f) ignoredFiles then pure [] else testsFromFile f problematicTests)) jsonFiles
  liftIO $ putStrLn "Loaded."
  pure $ testGroup "ethereum-tests" groups

testsFromFile
  :: forall m . App m
  => String -> Map String (TestTree -> TestTree) -> m [TestTree]
testsFromFile file problematicTests = do
  parsed <- parseBCSuite <$> (liftIO $ LazyByteString.readFile file)
  case parsed of
   Left "No cases to check." -> pure [] -- error "no-cases ok"
   Left _err -> pure [] -- error _err
   Right allTests -> mapM runTest (Map.toList allTests)
  where
    runTest :: (String , Case) -> m TestTree
    runTest (name, x) = do
      exec <- toIO $ runVMTest True (name, x)
      pure $ testCase' name exec
    testCase' :: String -> Assertion -> TestTree
    testCase' name assertion =
      case Map.lookup name problematicTests of
        Just f -> f (testCase name (liftIO assertion))
        Nothing -> testCase name (liftIO assertion)

-- CI has issues with some heaver tests, disable in bulk
ciIgnoredFiles :: [String]
ciIgnoredFiles = []

commonProblematicTests :: Map String (TestTree -> TestTree)
commonProblematicTests = Map.fromList
  [ ("loopMul_d0g0v0_Shanghai", ignoreTestBecause "hevm is too slow")
  , ("loopMul_d1g0v0_Shanghai", ignoreTestBecause "hevm is too slow")
  , ("loopMul_d2g0v0_Shanghai", ignoreTestBecause "hevm is too slow")
  , ("CALLBlake2f_MaxRounds_d0g0v0_Shanghai", ignoreTestBecause "very slow, bypasses timeout due time spent in FFI")
  ]

ciProblematicTests :: Map String (TestTree -> TestTree)
ciProblematicTests = Map.fromList
  [ ("Return50000_d0g1v0_Shanghai", ignoreTest)
  , ("Return50000_2_d0g1v0_Shanghai", ignoreTest)
  , ("randomStatetest177_d0g0v0_Shanghai", ignoreTest)
  , ("static_Call50000_d0g0v0_Shanghai", ignoreTest)
  , ("static_Call50000_d1g0v0_Shanghai", ignoreTest)
  , ("static_Call50000bytesContract50_1_d1g0v0_Shanghai", ignoreTest)
  , ("static_Call50000bytesContract50_2_d1g0v0_Shanghai", ignoreTest)
  , ("static_Return50000_2_d0g0v0_Shanghai", ignoreTest)
  , ("loopExp_d10g0v0_Shanghai", ignoreTest)
  , ("loopExp_d11g0v0_Shanghai", ignoreTest)
  , ("loopExp_d12g0v0_Shanghai", ignoreTest)
  , ("loopExp_d13g0v0_Shanghai", ignoreTest)
  , ("loopExp_d14g0v0_Shanghai", ignoreTest)
  , ("loopExp_d8g0v0_Shanghai", ignoreTest)
  , ("loopExp_d9g0v0_Shanghai", ignoreTest)
  ]

runVMTest
  :: App m
  => Bool -> (String, Case) -> m ()
runVMTest diffmode (_name, x) = do
  vm0 <- liftIO $ vmForCase x
  result <- EVM.Stepper.interpret (EVM.Fetch.zero 0 (Just 0)) vm0 EVM.Stepper.runFully
  writeTrace result
  maybeReason <- checkExpectation diffmode x result
  liftIO $ forM_ maybeReason assertFailure


-- | Run a vm test and output a geth style per opcode trace
traceVMTest
  :: App m
  => String -> String -> m [VMTrace]
traceVMTest file test = do
  repo <- liftIO $ getEnv "HEVM_ETHEREUM_TESTS_REPO"
  allTests <- parseBCSuite <$> (liftIO $ LazyByteString.readFile (repo </> file))
  let x = case filter (\(name, _) -> name == test) $ Map.toList (getRight allTests) of
        [(_, x')] -> x'
        _ -> internalError "test not found"
  vm0 <- liftIO $ vmForCase x
  (_, (_, ts)) <- runStateT (interpretWithTrace (EVM.Fetch.zero 0 (Just 0)) EVM.Stepper.runFully) (vm0, [])
  pure ts
    where
      getRight :: Either a b -> b
      getRight (Right a) = a
      getRight (Left _) = error "Not allowed"

-- | Read a geth trace from disk
readTrace :: FilePath -> IO (Either String EVMToolTraceOutput)
readTrace = JSON.eitherDecodeFileStrict

-- | given a path to a test file, a test case from within that file, and a trace from geth from running that test, compare the traces and show where we differ
-- This would need a few tweaks to geth to make this really usable (i.e. evm statetest show allow running a single test from within the test file).
traceVsGeth
  :: App m
  => String -> String -> FilePath -> m ()
traceVsGeth file test gethTrace = do
  hevm <- traceVMTest file test
  decodedContents <- liftIO (JSON.decodeFileStrict gethTrace :: IO (Maybe EVMToolTraceOutput))
  let EVMToolTraceOutput ts _ = fromJust decodedContents
  _ <- liftIO $ compareTraces hevm ts
  pure ()

splitEithers :: (Filterable f) => f (Either a b) -> (f a, f b)
splitEithers =
  (catMaybes *** catMaybes)
  . (fmap fst &&& fmap snd)
  . (fmap (preview _Left &&& preview _Right))

fromConcrete :: Expr Storage -> Map W256 W256
fromConcrete (ConcreteStore s) = s
fromConcrete s = internalError $ "unexpected abstract store: " <> show s

checkStateFail :: Bool -> Case -> VM Concrete RealWorld -> (Bool, Bool, Bool, Bool) -> IO String
checkStateFail diff x vm (okMoney, okNonce, okData, okCode) = do
  let
    printContracts :: Map Addr Contract -> IO ()
    printContracts cs = putStrLn $ Map.foldrWithKey (\k c acc ->
      acc ++ show k ++ " : "
                   ++ (show $ fromJust $ c.nonce) ++ " "
                   ++ (show $ fromJust $ maybeLitWord c.balance) ++ " "
                   ++ (show $ fromConcrete $ c.storage)
        ++ "\n") "" cs

    reason = map fst (filter (not . snd)
        [ ("bad-state",       okMoney || okNonce || okData  || okCode)
        , ("bad-balance", not okMoney || okNonce || okData  || okCode)
        , ("bad-nonce",   not okNonce || okMoney || okData  || okCode)
        , ("bad-storage", not okData  || okMoney || okNonce || okCode)
        , ("bad-code",    not okCode  || okMoney || okNonce || okData)
        ])
    check = x.checkContracts
    expected = x.testExpectation
    actual = fmap (clearZeroStorage . clearOrigStorage) $ forceConcreteAddrs vm.env.contracts

  when diff $ do
    putStr (unwords reason)
    putStrLn "\nPre balance/state: "
    printContracts check
    putStrLn "\nExpected balance/state: "
    printContracts expected
    putStrLn "\nActual balance/state: "
    printContracts actual
  pure (unwords reason)

checkExpectation
  :: App m
  => Bool -> Case -> VM Concrete RealWorld -> m (Maybe String)
checkExpectation diff x vm = do
  let expectation = x.testExpectation
      (okState, b2, b3, b4, b5) = checkExpectedContracts vm expectation
  if okState then
    pure Nothing
  else liftIO $
    Just <$> checkStateFail diff x vm (b2, b3, b4, b5)

-- quotient account state by nullness
(~=) :: Map Addr Contract -> Map Addr Contract -> Bool
(~=) cs1 cs2 =
    let nullAccount = EVM.initialContract (RuntimeCode (ConcreteRuntimeCode ""))
        padNewAccounts cs ks = Map.union cs $ Map.fromList [(k, nullAccount) | k <- ks]
        padded_cs1 = padNewAccounts cs1 (Map.keys cs2)
        padded_cs2 = padNewAccounts cs2 (Map.keys cs1)
    in and $ zipWith (===) (Map.elems padded_cs1) (Map.elems padded_cs2)

(===) :: Contract -> Contract -> Bool
c1 === c2 =
  codeEqual && storageEqual && (c1 ^. #balance == c2 ^. #balance) && (c1 ^. #nonce ==  c2 ^. #nonce)
  where
    storageEqual = c1.storage == c2.storage
    codeEqual = case (c1 ^. #code, c2 ^. #code) of
      (RuntimeCode a', RuntimeCode b') -> a' == b'
      _ -> internalError "unexpected code"

checkExpectedContracts :: VM Concrete RealWorld -> Map Addr Contract -> (Bool, Bool, Bool, Bool, Bool)
checkExpectedContracts vm expected =
  let cs = fmap (clearZeroStorage . clearOrigStorage) $ forceConcreteAddrs vm.env.contracts
  in ( (expected ~= cs)
     , (clearBalance <$> expected) ~= (clearBalance <$> cs)
     , (clearNonce   <$> expected) ~= (clearNonce   <$> cs)
     , (clearStorage <$> expected) ~= (clearStorage <$> cs)
     , (clearCode    <$> expected) ~= (clearCode    <$> cs)
     )

clearOrigStorage :: Contract -> Contract
clearOrigStorage = set #origStorage (ConcreteStore mempty)

clearZeroStorage :: Contract -> Contract
clearZeroStorage c = case c.storage of
  ConcreteStore m -> let store = Map.filter (/= 0) m
                     in set #storage (ConcreteStore store) c
  _ -> internalError "Internal Error: unexpected abstract store"

clearStorage :: Contract -> Contract
clearStorage c = c { storage = clear c.storage }
  where
    clear :: Expr Storage -> Expr Storage
    clear (ConcreteStore _) = ConcreteStore mempty
    clear _ = internalError "Internal Error: unexpected abstract store"

clearBalance :: Contract -> Contract
clearBalance c = set #balance (Lit 0) c

clearNonce :: Contract -> Contract
clearNonce c = set #nonce (Just 0) c

clearCode :: Contract -> Contract
clearCode c = set #code (RuntimeCode (ConcreteRuntimeCode "")) c

instance FromJSON Contract where
  parseJSON (JSON.Object v) = do
    code <- (RuntimeCode . ConcreteRuntimeCode <$> (hexText <$> v .: "code"))
    storage <- v .: "storage"
    balance <- v .: "balance"
    nonce   <- v .: "nonce"
    pure $ EVM.initialContract code
             & #balance .~ (Lit balance)
             & #nonce   ?~ nonce
             & #storage .~ (ConcreteStore storage)
             & #origStorage .~ (ConcreteStore storage)

  parseJSON invalid =
    JSON.typeMismatch "Contract" invalid

instance FromJSON BlockchainCase where
  parseJSON (JSON.Object v) = BlockchainCase
    <$> v .: "blocks"
    <*> parseContracts Pre v
    <*> parseContracts Post v
    <*> v .: "network"
  parseJSON invalid =
    JSON.typeMismatch "GeneralState test case" invalid

instance FromJSON Block where
  parseJSON (JSON.Object v) = do
    v'         <- v .: "blockHeader"
    txs        <- v .: "transactions"
    coinbase   <- addrField v' "coinbase"
    difficulty <- wordField v' "difficulty"
    gasLimit   <- word64Field v' "gasLimit"
    number     <- wordField v' "number"
    baseFee    <- fmap read <$> v' .:? "baseFeePerGas"
    timestamp  <- wordField v' "timestamp"
    mixHash    <- wordField v' "mixHash"
    pure $ Block coinbase difficulty mixHash gasLimit (fromMaybe 0 baseFee) number timestamp txs
  parseJSON invalid =
    JSON.typeMismatch "Block" invalid

parseContracts :: Which -> JSON.Object -> JSON.Parser (Map Addr Contract)
parseContracts w v = v .: which >>= parseJSON
  where which = case w of
          Pre  -> "pre"
          Post -> "postState"

parseBCSuite :: Lazy.ByteString -> Either String (Map String Case)
parseBCSuite x = case (JSON.eitherDecode' x) :: Either String (Map String BlockchainCase) of
  Left e        -> Left e
  Right bcCases -> let allCases = fromBlockchainCase <$> bcCases
                       keepError (Left e) = errorFatal e
                       keepError _        = True
                       filteredCases = Map.filter keepError allCases
                       (erroredCases, parsedCases) = splitEithers filteredCases
    in if Map.size erroredCases > 0
    then Left ("errored case: " ++ (show erroredCases))
    else if Map.size parsedCases == 0
    then Left "No cases to check."
    else Right parsedCases


data BlockchainError
  = TooManyBlocks
  | TooManyTxs
  | NoTxs
  | SignatureUnverified
  | InvalidTx
  | OldNetwork
  | FailedCreate
  deriving Show

errorFatal :: BlockchainError -> Bool
errorFatal TooManyBlocks = True
errorFatal TooManyTxs = True
errorFatal SignatureUnverified = True
errorFatal InvalidTx = True
errorFatal _ = False

fromBlockchainCase :: BlockchainCase -> Either BlockchainError Case
fromBlockchainCase (BlockchainCase blocks preState postState network) =
  case (blocks, network) of
    ([block], "Shanghai") -> case block.txs of
      [tx] -> fromBlockchainCase' block tx preState postState
      []        -> Left NoTxs
      _         -> Left TooManyTxs
    ([_], _) -> Left OldNetwork
    (_, _)   -> Left TooManyBlocks

maxCodeSize :: W256
maxCodeSize = 24576

fromBlockchainCase' :: Block -> Transaction
                       -> Map Addr Contract -> Map Addr Contract
                       -> Either BlockchainError Case
fromBlockchainCase' block tx preState postState =
  let isCreate = isNothing tx.toAddr in
  case (sender tx, checkTx tx block preState) of
      (Nothing, _) -> Left SignatureUnverified
      (_, Nothing) -> Left (if isCreate then FailedCreate else InvalidTx)
      (Just origin, Just checkState) -> Right $ Case
        (VMOpts
         { contract       = EVM.initialContract theCode
         , otherContracts = []
         , calldata       = (cd, [])
         , value          = Lit tx.value
         , address        = toAddr
         , caller         = LitAddr origin
         , baseState      = EmptyBase
         , origin         = LitAddr origin
         , gas            = tx.gasLimit - txGasCost feeSchedule tx
         , baseFee        = block.baseFee
         , priorityFee    = priorityFee tx block.baseFee
         , gaslimit       = tx.gasLimit
         , number         = block.number
         , timestamp      = Lit block.timestamp
         , coinbase       = LitAddr block.coinbase
         , prevRandao     = block.mixHash
         , maxCodeSize    = maxCodeSize
         , blockGaslimit  = block.gasLimit
         , gasprice       = effectiveGasPrice
         , schedule       = feeSchedule
         , chainId        = 1
         , create         = isCreate
         , txAccessList   = Map.mapKeys LitAddr (txAccessMap tx)
         , allowFFI       = False
         })
        checkState
        postState
          where
            toAddr = maybe (EVM.createAddress origin (fromJust senderNonce)) LitAddr (tx.toAddr)
            senderNonce = view (accountAt (LitAddr origin) % #nonce) (Map.mapKeys LitAddr preState)
            toCode = Map.lookup toAddr (Map.mapKeys LitAddr preState)
            theCode = if isCreate
                      then InitCode tx.txdata mempty
                      else maybe (RuntimeCode (ConcreteRuntimeCode "")) (view #code) toCode
            effectiveGasPrice = effectiveprice tx block.baseFee
            cd = if isCreate
                 then mempty
                 else ConcreteBuf tx.txdata

effectiveprice :: Transaction -> W256 -> W256
effectiveprice tx baseFee = priorityFee tx baseFee + baseFee

priorityFee :: Transaction -> W256 -> W256
priorityFee tx baseFee = let
    (txPrioMax, txMaxFee) = case tx.txtype of
               EIP1559Transaction ->
                 let maxPrio = fromJust tx.maxPriorityFeeGas
                     maxFee = fromJust tx.maxFeePerGas
                 in (maxPrio, maxFee)
               _ ->
                 let gasPrice = fromJust tx.gasPrice
                 in (gasPrice, gasPrice)
  in min txPrioMax (txMaxFee - baseFee)

maxBaseFee :: Transaction -> W256
maxBaseFee tx =
  case tx.txtype of
     EIP1559Transaction -> fromJust tx.maxFeePerGas
     _ -> fromJust tx.gasPrice

validateTx :: Transaction -> Block -> Map Addr Contract -> Maybe ()
validateTx tx block cs = do
  origin        <- sender tx
  (Lit originBalance) <- (view #balance) <$> view (at origin) cs
  originNonce   <- (view #nonce)   <$> view (at origin) cs
  let gasDeposit = (effectiveprice tx block.baseFee) * (into tx.gasLimit)
  if gasDeposit + tx.value <= originBalance
    && (Just (unsafeInto tx.nonce) == originNonce) && block.baseFee <= maxBaseFee tx
  then Just ()
  else Nothing

checkTx :: Transaction -> Block -> Map Addr Contract -> Maybe (Map Addr Contract)
checkTx tx block prestate = do
  origin <- sender tx
  validateTx tx block prestate
  let isCreate    = isNothing tx.toAddr
      cs          = Map.mapKeys LitAddr prestate
      senderNonce = view (accountAt (LitAddr origin) % #nonce) cs
      toAddr      = maybe (EVM.createAddress origin (fromJust senderNonce)) LitAddr (tx.toAddr)
      prevCode    = view (accountAt toAddr % #code) cs
      prevNonce   = view (accountAt toAddr % #nonce) cs

      nonEmptyAccount = case prevCode of
                        RuntimeCode (ConcreteRuntimeCode b) -> not (BS.null b)
                        _ -> True
      badNonce = prevNonce /= Just 0
      initCodeSizeExceeded = BS.length tx.txdata > (unsafeInto maxCodeSize * 2)
  if isCreate && (badNonce || nonEmptyAccount || initCodeSizeExceeded)
  then mzero
  else
    pure prestate

vmForCase :: Case -> IO (VM Concrete RealWorld)
vmForCase x = do
  vm <- stToIO $ makeVm x.vmOpts
    <&> set (#env % #contracts) (Map.mapKeys LitAddr x.checkContracts)
  pure $ initTx vm

forceConcreteAddrs :: Map (Expr EAddr) Contract -> Map Addr Contract
forceConcreteAddrs cs = Map.mapKeys
      (fromMaybe (internalError "Internal Error: unexpected symbolic address") . maybeLitAddr)
      cs
