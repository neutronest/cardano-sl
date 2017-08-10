-- | Utility functions for Pos.Client.Txp

module Test.Pos.Client.Txp.Util
       ( seedSize
       , generateAddressAux
       , generateTxOutAux
       , makeDummyUtxo
       ) where

import           Universum

import           Unsafe     (unsafeFromJust)

import qualified Data.Text  as T
import           Formatting (build, sformat, (%))

import           Pos.Core   (makePubKeyAddressBoot, unsafeIntegerToCoin)
import           Pos.Crypto (SecretKey, decodeHash, deterministicKeyGen)
import           Pos.Txp    (TxId, TxIn (..), TxOut (..), TxOutAux (..), Utxo)
import           Pos.Types  (Address)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

seedSize :: Int
seedSize = 32

generateAddressAux :: ByteString -> (SecretKey, Address)
generateAddressAux seed =
    if length seed /= seedSize then
        error $ sformat ("Internal error: seed size must be exactly "%build%" bytes") seedSize
    else
        let (pk, sk) = deterministicKeyGen seed
        in (sk, makePubKeyAddressBoot pk)

generateTxOutAux :: Integer -> ByteString -> (SecretKey, Address, TxOutAux)
generateTxOutAux amount seed =
    let (sk, addr) = generateAddressAux seed
        coin       = unsafeIntegerToCoin amount
        txOut      = TxOut addr coin
    in (sk, addr, TxOutAux txOut)

makeDummyUtxo :: TxOutAux -> Utxo
makeDummyUtxo txOutAux = one (txInDummy, txOutAux)
  where
    txIdDummy :: TxId
    txIdDummy = unsafeFromJust $ rightToMaybe $ decodeHash $ T.replicate 64 "0"

    txInDummy :: TxIn
    txInDummy = TxInUtxo txIdDummy 0
