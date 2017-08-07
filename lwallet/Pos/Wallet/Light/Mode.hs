{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

-- | Stack of monads used by light wallet.

module Pos.Wallet.Light.Mode
       ( LightWalletMode
       , LightWalletContext(..)
       ) where

import           Universum

import           Control.Lens                     (makeLensesWith)
import qualified Control.Monad.Reader             as Mtl
import           Ether.Internal                   (HasLens (..))
import           Mockable                         (Production)
import           System.Wlog                      (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener              (MonadBListener (..), onApplyBlocksStub,
                                                   onRollbackBlocksStub)
import           Pos.Block.Core                   (Block, BlockHeader)
import           Pos.Block.Types                  (Undo)
import           Pos.Client.Txp.Balances          (MonadBalances (..), getBalanceDefault,
                                                   getOwnUtxosDefault)
import           Pos.Client.Txp.History           (MonadTxHistory (..),
                                                   getBlockHistoryDefault,
                                                   getLocalHistoryDefault)
import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.Core                         (HasPrimaryKey (..), IsHeader,
                                                   SlotId (..))
import           Pos.DB                           (MonadGState (..))
import           Pos.DB.Block                     (dbGetBlockDefault,
                                                   dbGetBlockSscDefault,
                                                   dbGetHeaderDefault,
                                                   dbGetHeaderSscDefault,
                                                   dbGetUndoDefault, dbGetUndoSscDefault,
                                                   dbPutBlundDefault)
import           Pos.DB.Class                     (MonadBlockDBGeneric (..),
                                                   MonadBlockDBGenericWrite (..),
                                                   MonadDB (..), MonadDBRead (..))
import           Pos.DB.DB                        (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks                     (NodeDBs, dbDeleteDefault, dbGetDefault,
                                                   dbIterSourceDefault, dbPutDefault,
                                                   dbWriteBatchDefault)
import           Pos.Discovery                    (MonadDiscovery (..))
import           Pos.Reporting.MemState           (ReportingContext)
import           Pos.Slotting                     (MonadSlots (..),
                                                   currentTimeSlottingSimple)
import           Pos.Slotting.MemState            (MonadSlotsData (..))
import           Pos.Ssc.Class.Types              (SscBlock)
import           Pos.Ssc.GodTossing               (SscGodTossing)
import           Pos.Txp                          (GenesisStakeholders)
import           Pos.Util                         (Some (..))
import           Pos.Util.JsonLog                 (HasJsonLogConfig (..), JsonLogConfig,
                                                   jsonLogDefault)
import           Pos.Util.LoggerName              (HasLoggerName' (..),
                                                   getLoggerNameDefault,
                                                   modifyLoggerNameDefault)
import           Pos.Util.TimeWarp                (CanJsonLog (..))
import           Pos.Util.UserSecret              (HasUserSecret (..))
import           Pos.Util.Util                    (postfixLFields)
import           Pos.Wallet.KeyStorage            (KeyData)
import           Pos.Wallet.Light.Redirect        (saveTxWallet)
import           Pos.Wallet.Light.State.Core      (gsAdoptedBVDataWallet)
import           Pos.Wallet.WalletMode            (MonadBlockchainInfo (..),
                                                   MonadUpdates (..))

type LightWalletSscType = SscGodTossing
-- type LightWalletSscType = SscNistBeacon

data LightWalletContext = LightWalletContext
    { lwcKeyData          :: !KeyData
    , lwcNodeDBs          :: !NodeDBs
    , lwcReportingContext :: !ReportingContext
    , lwcDiscoveryPeers   :: !(Set NodeId)
    , lwcJsonLogConfig    :: !JsonLogConfig
    , lwcLoggerName       :: !LoggerName
    , lwcGenStakeholders  :: !GenesisStakeholders
    }

makeLensesWith postfixLFields ''LightWalletContext

instance HasLens NodeDBs LightWalletContext NodeDBs where
    lensOf = lwcNodeDBs_L

type LightWalletMode = Mtl.ReaderT LightWalletContext Production

instance HasUserSecret LightWalletContext where
    userSecret = lwcKeyData_L

instance HasLens GenesisStakeholders LightWalletContext GenesisStakeholders where
    lensOf = lwcGenStakeholders_L

instance HasLoggerName' LightWalletContext where
    loggerName = lwcLoggerName_L

instance HasJsonLogConfig LightWalletContext where
    jsonLogConfig = lwcJsonLogConfig_L

instance {-# OVERLAPPING #-} HasLoggerName LightWalletMode where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog LightWalletMode where
    jsonLog = jsonLogDefault

instance MonadDiscovery LightWalletMode where
    getPeers = view lwcDiscoveryPeers_L
    findPeers = view lwcDiscoveryPeers_L

instance MonadBListener LightWalletMode where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

-- FIXME: Dummy instance for lite-wallet.
instance MonadBlockchainInfo LightWalletMode where
    networkChainDifficulty = error "notImplemented"
    localChainDifficulty = error "notImplemented"
    blockchainSlotDuration = error "notImplemented"
    connectedPeers = error "notImplemented"

-- FIXME: Dummy instance for lite-wallet.
instance MonadUpdates LightWalletMode where
    waitForUpdate = error "notImplemented"
    applyLastUpdate = pure ()

-- FIXME: Dummy instance for lite-wallet.
instance MonadSlotsData LightWalletMode where
    getSystemStart = error "notImplemented"
    getSlottingData = error "notImplemented"
    waitPenultEpochEquals = error "notImplemented"
    putSlottingData = error "notImplemented"

-- FIXME: Dummy instance for lite-wallet.
instance MonadSlots LightWalletMode where
    getCurrentSlot = Just <$> getCurrentSlotInaccurate
    getCurrentSlotBlocking = getCurrentSlotInaccurate
    getCurrentSlotInaccurate = pure (SlotId 0 minBound)
    currentTimeSlotting = currentTimeSlottingSimple

instance MonadDBRead LightWalletMode where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance MonadDB LightWalletMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

instance MonadBlockDBGenericWrite (BlockHeader LightWalletSscType) (Block LightWalletSscType) Undo LightWalletMode where
    dbPutBlund = dbPutBlundDefault

instance MonadBlockDBGeneric (BlockHeader LightWalletSscType) (Block LightWalletSscType) Undo LightWalletMode
  where
    dbGetBlock  = dbGetBlockDefault @LightWalletSscType
    dbGetUndo   = dbGetUndoDefault @LightWalletSscType
    dbGetHeader = dbGetHeaderDefault @LightWalletSscType

instance MonadBlockDBGeneric (Some IsHeader) (SscBlock LightWalletSscType) () LightWalletMode
  where
    dbGetBlock  = dbGetBlockSscDefault @LightWalletSscType
    dbGetUndo   = dbGetUndoSscDefault @LightWalletSscType
    dbGetHeader = dbGetHeaderSscDefault @LightWalletSscType

instance MonadGState LightWalletMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBalances LightWalletMode where
    getOwnUtxos = getOwnUtxosDefault
    getBalance = getBalanceDefault

instance MonadTxHistory LightWalletSscType LightWalletMode where
    getBlockHistory = getBlockHistoryDefault @LightWalletSscType
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxWallet
