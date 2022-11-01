import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet, addSomeWallet)
import Test.Plutip.LocalCluster (waitSeconds)
import Test.Plutip.Internal.Types

setup :: ReaderT ClusterEnv IO (ClusterEnv, BpiWallet)
setup = do
  env <- ask
  -- Gotta have all those utxos for the collaterals.
  ownWallet <- addWalletWithAdas $ 100 : replicate 20 10
  -- Wait for faucet funds to be added.
  waitSeconds 2
  pure (env, ownWallet)

addWalletWithAdas :: [Ada] -> ReaderT ClusterEnv IO BpiWallet
addWalletWithAdas = addSomeWallet . map (fromInteger . Ada.toLovelace)

import qualified Plutus.Contract as Contract
import qualified Ledger.Constraints as Constraints

-- | Pay 5 ada to yourself ...very useful thing to do
payMyself :: AsContractError e => Contract w s e ()
payMyself = do
  ownPkh <- Contract.ownPaymentPubKeyHash
  let tx = Constraints.mustPayToPubKey ownPkh $ Ada.toValue 5
  ledgerTx <- Contract.submitTxConstraintsWith @Void mempty tx
  void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx

runContract ::
  (ToJSON w, Monoid w, MonadIO m) =>
  ClusterEnv ->
  BpiWallet ->
  Contract w s e a ->
  m (ExecutionResult w e a)

data ExecutionResult w e a = ExecutionResult
  { -- | outcome of running contract.
    outcome :: Either (FailureReason e) a
  , -- | stats returned by bot interface after contract being run
    txStats :: ContractStats
  , -- | `Contract` observable state after execution (or up to the point where it failed)
    contractState :: w
  }
  deriving stock (Show)

data FailureReason e
  = -- | error thrown by `Contract` (via `throwError`)
    ContractExecutionError e
  | -- | exception caught during contract execution
    CaughtException SomeException
  deriving stock (Show)

ExecutionResult exOutcome _ _ <- runContract @() @EmptySchema @Text cEnv ownWallet payMyself

case exOutcome of
  Left (ContractExecutionError e) -> putStrLn "Contract failed" >> print e
  Left (CaughtException e) -> putStrLn "Unexpected exception" >> print e
  Right _ -> pure ()

main :: IO ()
main = do
  -- Start the node.
  (clusterStat, (cEnv, ownWallet)) <- startCluster def setup

  -- Do stuff.
  ExecutionResult exOutcome _ _ <- runContract @() @EmptySchema @Text cEnv ownWallet payMyself
  case exOutcome of
    Left (ContractExecutionError e) -> putStrLn "Contract failed" >> print e
    Left (CaughtException e) -> putStrLn "Unexpected exception" >> print e
    Right _ -> pure ()

  -- Stop the node.
  stopCluster clusterStat
