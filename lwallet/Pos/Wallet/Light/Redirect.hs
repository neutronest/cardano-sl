{-# LANGUAGE TypeFamilies #-}

-- | Various monadic redirects implementing wallet constraints. Mostly stubs.

module Pos.Wallet.Light.Redirect
       ( getOwnUtxosWallet
       , getBlockHistoryWallet
       , getLocalHistoryWallet
       , saveTxWallet
       ) where

import           Universum

import           Control.Lens              (has)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.DList                (DList)
import qualified Data.HashSet              as HS
import           Data.List                 (partition)
import qualified Data.Map                  as M
import           Ether.Internal            (HasLens (..))

import           Pos.Client.Txp.History    (TxHistoryEntry, deriveAddrHistory)
import           Pos.Core                  (Address)
import           Pos.Core                  (AddressIgnoringAttributes (AddressIA))
import           Pos.Core.Types            (_RedeemAddress)
import           Pos.DB.Class              (MonadDBRead (..), MonadGState)
import           Pos.DB.Rocks              (MonadRealDB)
import           Pos.Txp                   (MonadTxpMem, TxAux, TxId, Utxo,
                                            addrBelongsToSet, getUtxoModifier,
                                            runUtxoStateT)
import qualified Pos.Txp.DB                as DB
import qualified Pos.Util.Modifier         as MM
import qualified Pos.Wallet.Light.State    as LWS

----------------------------------------------------------------------------
-- MonadBalances
----------------------------------------------------------------------------

type LightBalancesEnv ext ctx m =
    ( MonadRealDB ctx m
    , MonadDBRead m
    , MonadGState m
    , MonadMask m
    , MonadTxpMem ext ctx m)

getOwnUtxosWallet :: LightBalancesEnv ext ctx m => [Address] -> m Utxo
getOwnUtxosWallet addrs = do
    let (redeemAddrs, _commonAddrs) = partition (has _RedeemAddress) addrs

    updates <- getUtxoModifier
    redeemUtxo <- if null redeemAddrs then pure mempty
                  else DB.getFilteredUtxo redeemAddrs

    let allUtxo = MM.modifyMap updates $ redeemUtxo
        addrsSet = HS.fromList $ AddressIA <$> addrs
    pure $ M.filter (`addrBelongsToSet` addrsSet) allUtxo


----------------------------------------------------------------------------
-- MonadTxHistory
----------------------------------------------------------------------------

getBlockHistoryWallet
    :: (MonadReader ctx m, HasLens LWS.WalletState ctx LWS.WalletState, MonadIO m)
    => [Address] -> m (DList TxHistoryEntry)
getBlockHistoryWallet addrs = do
    chain <- LWS.getBestChain
    utxo <- LWS.getOldestUtxo
    _ <- fmap (fst . fromMaybe (error "deriveAddrHistory: Nothing")) $
        runMaybeT $ flip runUtxoStateT utxo $
        deriveAddrHistory addrs chain
    pure $ error "getBlockHistory is not implemented for light wallet"

getLocalHistoryWallet
    :: [Address] -> m (DList TxHistoryEntry)
getLocalHistoryWallet = pure $
    error "getLocalHistory is not implemented for light wallet"

saveTxWallet :: Monad m => (TxId, TxAux) -> m ()
saveTxWallet _ = pure ()
