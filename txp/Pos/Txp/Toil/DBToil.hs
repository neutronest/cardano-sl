{-# LANGUAGE TypeFamilies #-}

-- | Instances of 'MonadUtxoRead', 'MonadBalancesRead' which use DB.

module Pos.Txp.Toil.DBToil
       ( DBToil
       , runDBToil
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Data.HashMap.Strict          as HM
import qualified Data.Map                     as M
import qualified Ether

import           Pos.DB.Class                 (MonadDBRead)
import           Pos.DB.GState.Balances       (getRealStake, getRealTotalStake)
import           Pos.Txp.DB.Utxo              (getAllPotentiallyHugeUtxo, getTxOut)
import           Pos.Txp.Toil.Class           (MonadBalancesRead (..), MonadUtxoRead (..))

data DBToilTag

type DBToil = Ether.TaggedTrans DBToilTag IdentityT

runDBToil :: DBToil m a -> m a
runDBToil = coerce

instance (MonadDBRead m) => MonadUtxoRead (DBToil m) where
    utxoGet = getTxOut
    getFullUtxo = (HM.fromList . M.toList) <$> getAllPotentiallyHugeUtxo

instance (MonadDBRead m) => MonadBalancesRead (DBToil m) where
    getTotalStake = getRealTotalStake
    getStake = getRealStake
