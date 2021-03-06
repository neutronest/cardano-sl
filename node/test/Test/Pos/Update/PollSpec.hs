-- | Specification for submodules of Pos.Update.Poll

module Test.Pos.Update.PollSpec
       ( spec
       ) where

import           Universum

import           Control.Lens                      (at)
import qualified Data.HashSet                      as HS
import           Test.Hspec                        (Spec, describe)
import           Test.Hspec.QuickCheck             (modifyMaxSuccess, prop)
import           Test.QuickCheck                   (Arbitrary (..), Gen, Property,
                                                    conjoin, forAll, (===))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Core                          (ApplicationName, BlockVersion (..),
                                                    BlockVersionData (..),
                                                    HasCoreConstants,
                                                    SoftwareVersion (..), StakeholderId,
                                                    addressHash, giveStaticConsts)
import           Pos.Crypto                        (hash)
import           Pos.Slotting.Types                (SlottingData)
import           Pos.Update.Core                   (UpId, UpdateProposal (..), applyBVM)
import qualified Pos.Update.Poll                   as Poll
import qualified Pos.Util.Modifier                 as MM

import           Test.Pos.Util                     (formsMonoid)

spec :: Spec
spec = giveStaticConsts $ describe "Poll" $ do
    let smaller n = modifyMaxSuccess (const n)
    describe "modifyPollModifier" $ smaller 30 $ do
        prop
            "poll modifiers form a commutative monoid under 'modifyPollModifier'"
            modifyPollFormsMonoid
    describe "PollState" $ smaller 30 $ do
        prop
            "applying two poll modifiers in sequence to the poll state is equivalent\
            \ to combining them and applying the resulting modifier"
            modifyPollStateWithModifiers
        describe "PurePoll" $ smaller 30 $ do
            prop
                "applying a series of modifications to a modifier and then applying it to\
                \ a poll state is the same as applying the modifications directly to the\
                \ poll state"
                applyActions
            prop "Adding and then deleting a block version's state to 'PollState' is\
                 \ equivalent to doing nothing"
                 putDelBVState
            prop "Setting and then deleting the last confirmed version of an application\
                 \ is equivalent to doing nothing"
                 setDeleteConfirmedSV
            prop "Adding and then deleting a confirmed proposal is the same as doing\
                 \ nothing"
                 addDeleteConfirmedProposal
            prop "Inserting an active proposal and then deleting it is the same as doing\
                 \ nothing"
                 insertDeleteProposal

modifyPollFormsMonoid
    :: Poll.PollModifier
    -> Poll.PollModifier
    -> Poll.PollModifier
    -> Property
modifyPollFormsMonoid = formsMonoid

modifyPollStateWithModifiers
    :: Poll.PollState
    -> Poll.PollModifier
    -> Poll.PollModifier
    -> Property
modifyPollStateWithModifiers pst pm1 pm2 =
    Poll.modifyPollState pm2 (Poll.modifyPollState pm1 pst) ===
    Poll.modifyPollState (pm1 <> pm2) pst

data PollAction
    = PutBVState BlockVersion Poll.BlockVersionState
    | DelBVState BlockVersion
    | SetAdoptedBV BlockVersion
    | SetLastConfirmedSV SoftwareVersion
    | DelConfirmedSV ApplicationName
    | AddConfirmedProposal Poll.ConfirmedProposalState
    | DelConfirmedProposal SoftwareVersion
    | InsertActiveProposal Poll.ProposalState
    | DeactivateProposal UpId
    | SetSlottingData SlottingData
    | SetEpochProposers (HashSet StakeholderId)
    deriving (Show, Eq, Generic)

instance HasCoreConstants => Arbitrary PollAction where
    arbitrary = genericArbitrary
    shrink = genericShrink

actionToMonad :: Poll.MonadPoll m => PollAction -> m ()
actionToMonad (PutBVState bv bvs)        = Poll.putBVState bv bvs
actionToMonad (DelBVState bv)            = Poll.delBVState bv
actionToMonad (SetAdoptedBV bv)          = Poll.setAdoptedBV bv
actionToMonad (SetLastConfirmedSV sv)    = Poll.setLastConfirmedSV sv
actionToMonad (DelConfirmedSV an)        = Poll.delConfirmedSV an
actionToMonad (AddConfirmedProposal cps) = Poll.addConfirmedProposal cps
actionToMonad (DelConfirmedProposal sv)  = Poll.delConfirmedProposal sv
actionToMonad (InsertActiveProposal ps)  = Poll.insertActiveProposal ps
actionToMonad (DeactivateProposal ui)    = Poll.deactivateProposal ui
actionToMonad (SetSlottingData sd)       = Poll.setSlottingData sd
actionToMonad (SetEpochProposers hs)     = Poll.setEpochProposers hs

applyActionToModifier
    :: PollAction
    -> Poll.PollState
    -> Poll.PollModifier
    -> Poll.PollModifier
applyActionToModifier (PutBVState bv bvs) _ = Poll.pmBVsL %~ MM.insert bv bvs
applyActionToModifier (DelBVState bv) _ = Poll.pmBVsL %~ MM.delete bv
applyActionToModifier (SetAdoptedBV bv) pst = \pm -> do
    let adoptedBVData = snd $
            fromMaybe (pst ^. Poll.psAdoptedBV) (Poll.pmAdoptedBVFull pm)
    case MM.lookup innerLookupFun bv (Poll.pmBVs pm) of
        Nothing                    -> pm
        Just (Poll.bvsModifier -> bvm) ->
            pm { Poll.pmAdoptedBVFull = Just (bv, applyBVM bvm adoptedBVData) }
  where
    innerLookupFun k = pst ^. Poll.psBlockVersions . at k
applyActionToModifier (SetLastConfirmedSV SoftwareVersion {..}) _ =
    Poll.pmConfirmedL %~ MM.insert svAppName svNumber
applyActionToModifier (DelConfirmedSV an) _ = Poll.pmConfirmedL %~ MM.delete an
applyActionToModifier (AddConfirmedProposal cps) _ =
    Poll.pmConfirmedPropsL %~ MM.insert (Poll.cpsSoftwareVersion cps) cps
applyActionToModifier (DelConfirmedProposal sv) _ = Poll.pmConfirmedPropsL %~ MM.delete sv
applyActionToModifier (InsertActiveProposal ps) pst = \p ->
    let up@UnsafeUpdateProposal{..} = Poll.psProposal ps
        upId = hash up
        p' = case MM.lookup innerLookupFun upId (Poll.pmActiveProps p) of
            Nothing -> p
            Just _ -> p & Poll.pmEpochProposersL %~ fmap (HS.insert (addressHash upFrom))
    in p' & (Poll.pmActivePropsL %~ MM.insert upId ps)
  where
    innerLookupFun k = pst ^. Poll.psActiveProposals . at k

applyActionToModifier (DeactivateProposal ui) pst = \p ->
    let proposal = MM.lookup innerLookupFun ui (Poll.pmActiveProps p)
    in case proposal of
           Nothing -> p
           Just ps ->
               let up = Poll.psProposal ps
                   upId = hash up
               in p & (Poll.pmActivePropsL %~ MM.delete upId)
  where
    innerLookupFun k = pst ^. Poll.psActiveProposals . at k

applyActionToModifier (SetSlottingData sd) _   = Poll.pmSlottingDataL .~ (Just sd)
applyActionToModifier (SetEpochProposers hs) _ = Poll.pmEpochProposersL .~ (Just hs)

applyActions :: Poll.PollState -> [PollAction] -> Property
applyActions ps actionList =
    let pollSts = fmap (actionToMonad @Poll.PurePoll) actionList
        -- 'resultModifiers' has a 'mempty' poll modifier up front, so 'newPollStates'
        -- has two 'ps's in the head of the list. As such another 'ps' is added
        -- at the head of 'resultPStates' to make up for that.
        resultModifiers =
            scanl (\pmod act -> applyActionToModifier act ps pmod) mempty actionList
        resultPStates = ps : scanl Poll.execPurePollWithLogger ps pollSts
        newPollStates = scanl (flip Poll.modifyPollState) ps resultModifiers
    in conjoin $ zipWith (===) resultPStates newPollStates

-- | Type synonym used for convenience.
type PollStateTestInfo = (BlockVersion, BlockVersionData)

-- | Empty 'PollState' to be used in tests. Since all fields of the datatype except
-- the second (psAdoptedBV) have an instance for 'Monoid', it is passed as an argument
-- that each property will supply.
emptyPollSt :: PollStateTestInfo -> Poll.PollState
emptyPollSt bvInfo = Poll.PollState
    mempty
    bvInfo
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty

-- | Apply a sequence of 'PollAction's from left to right.
perform :: [PollAction] -> Poll.PurePoll ()
perform = foldl (>>) (return ()) . map actionToMonad

-- | Operational equivalence operator in the 'PurePoll' monad. To be used when
-- equivalence between two sequences of actions in 'PurePoll' is to be tested/proved.
(==^)
    :: HasCoreConstants
    => [PollAction]
    -> [PollAction]
    -> PollStateTestInfo
    -> Property
p1 ==^ p2 = \bvInfo ->
    forAll (arbitrary :: Gen [PollAction]) $ \prefix ->
    forAll (arbitrary :: Gen [PollAction]) $ \suffix ->
        let applyAction x =
                Poll.execPurePollWithLogger (emptyPollSt bvInfo)
                                            (perform $ prefix ++ x ++ suffix)
        in applyAction p1 === applyAction p2

putDelBVState
    :: HasCoreConstants
    => BlockVersion
    -> Poll.BlockVersionState
    -> PollStateTestInfo
    -> Property
putDelBVState bv bvs = [PutBVState bv bvs, DelBVState bv] ==^ []

setDeleteConfirmedSV
    :: HasCoreConstants
    => SoftwareVersion
    -> PollStateTestInfo
    -> Property
setDeleteConfirmedSV sv = [SetLastConfirmedSV sv, DelConfirmedSV $ svAppName sv] ==^ []

addDeleteConfirmedProposal
    :: HasCoreConstants
    => Poll.ConfirmedProposalState
    -> PollStateTestInfo
    -> Property
addDeleteConfirmedProposal cps =
    [AddConfirmedProposal cps, DelConfirmedProposal $ Poll.cpsSoftwareVersion cps] ==^ []

insertDeleteProposal
    :: HasCoreConstants
    => Poll.ProposalState
    -> PollStateTestInfo
    -> Property
insertDeleteProposal ps =
    [InsertActiveProposal ps, DeactivateProposal $ hash $ Poll.psProposal ps] ==^ []
