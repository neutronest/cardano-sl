-- | Specification of Pos.Client.Txp.Util

module Test.Pos.Client.Txp.UtilSpec
       ( spec
       ) where

import           Universum

import qualified Data.ByteString          as BS
import qualified Data.List.NonEmpty       as NE
import           Formatting               (build, sformat, (%))
import           Test.Hspec               (Spec, describe)
import           Test.Hspec.QuickCheck    (prop)

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Util      (TxError (..), TxOutputs, TxWithSpendings,
                                           createMTx)
import           Pos.Core                 (HasCoreConstants)
import           Pos.Crypto               (SafeSigner, fakeSigner)
import           Pos.Txp                  (TxOutAux (..), Utxo)
import           Pos.Types                (Address)
import           Test.Pos.Util            (giveTestsConsts, stopProperty)

import           Test.Pos.Client.Txp.Mode (TxpTestMode, TxpTestProperty)
import           Test.Pos.Client.Txp.Util (generateTxOutAux, makeDummyUtxo, seedSize)

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

spec :: Spec
spec = giveTestsConsts $ describe "Client.Txp.Util" $ do
    describe "createMTx" $ createMTxSpec

createMTxSpec :: HasCoreConstants => Spec
createMTxSpec = do
    prop createMTxWorksWhenWeAreRichDesc createMTxWorksWhenWeAreRichSpec
    prop stabilizationDoesNotFailDesc stabilizationDoesNotFailSpec
    prop feeIsNonzeroDesc feeIsNonzeroSpec
  where
    createMTxWorksWhenWeAreRichDesc =
        "Transaction is created successfully when we have 1 input with 1M coins and 1 output with 1 coin"
    stabilizationDoesNotFailDesc =
        "The message \"Couldn't stabilize tx fee after 5 attempts\" is not thrown when there is " <>
        "1 input with 200k coins and 1 output with 1 coin"
    feeIsNonzeroDesc =
        "An attempt to create a tx for 1 coin when you have 1 coin fails because of the fee"

createMTxWorksWhenWeAreRichSpec :: HasCoreConstants => TxpTestProperty ()
createMTxWorksWhenWeAreRichSpec = do
    txOrError <- createMTx cmpUtxo cmpSigners cmpOutputs cmpAddrData
    case txOrError of
        Left err -> stopProperty $ sformat ("Failed to create tx: "%build) err
        Right tx -> ensureTxMakesSense tx cmpTxOutAuxInput cmpTxOutAuxOutput
  where
    CreateMTxParams {..} = make1to1Params 1000000 1

stabilizationDoesNotFailSpec :: HasCoreConstants => TxpTestProperty ()
stabilizationDoesNotFailSpec = do
    txOrError <- createMTx cmpUtxo cmpSigners cmpOutputs cmpAddrData
    case txOrError of
        Left err@(FailedToStabilize _) -> stopProperty $ pretty err
        Left _ -> return ()
        Right tx -> ensureTxMakesSense tx cmpTxOutAuxInput cmpTxOutAuxOutput
  where
    CreateMTxParams {..} = make1to1Params 200000 1

feeIsNonzeroSpec :: HasCoreConstants => TxpTestProperty ()
feeIsNonzeroSpec = do
    txOrError <- createMTx cmpUtxo cmpSigners cmpOutputs cmpAddrData
    case txOrError of
        Left (NotEnoughMoney _) -> return ()
        Left err -> stopProperty $ pretty err
        Right _ -> stopProperty $
            sformat ("Transaction was created even though there were not enough funds for the fee")
  where
    CreateMTxParams {..} = make1to1Params 1 1

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

data CreateMTxParams = CreateMTxParams
    { cmpUtxo           :: !Utxo
    , cmpSigners        :: !(NonEmpty (SafeSigner, Address))
    , cmpOutputs        :: !TxOutputs
    , cmpAddrData       :: !(AddrData TxpTestMode)
    -- The following two are not parameters of createMTx, however they are needed
    -- for verifying its output.
    , cmpTxOutAuxInput  :: !TxOutAux
    , cmpTxOutAuxOutput :: !TxOutAux
    }

make1to1Params :: Integer -> Integer -> CreateMTxParams
make1to1Params amountFrom amountTo = CreateMTxParams {..}
  where
    seedInput  = BS.replicate seedSize (0 :: Word8)
    seedOutput = BS.replicate seedSize (1 :: Word8)

    (sk, addr, cmpTxOutAuxInput) = generateTxOutAux amountFrom seedInput
    (_, _, cmpTxOutAuxOutput)    = generateTxOutAux amountTo   seedOutput

    cmpUtxo = makeDummyUtxo cmpTxOutAuxInput
    cmpSigners = one (fakeSigner sk, addr)
    cmpOutputs = one cmpTxOutAuxOutput
    cmpAddrData = ()

ensureTxMakesSense :: HasCoreConstants => TxWithSpendings -> TxOutAux -> TxOutAux -> TxpTestProperty ()
ensureTxMakesSense (_, neTxOut) toaInput _ = do
    unless (toaOut toaInput == NE.head neTxOut) $
        stopProperty $ "Returned TxOut is different from the initial one!"
