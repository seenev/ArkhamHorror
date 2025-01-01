module Arkham.Helpers.Log where

import Arkham.Prelude

import Arkham.Campaign.Types (Field (..))
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card.CardCode
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Data.Typeable

getCampaignLog :: HasGame m => m CampaignLog
getCampaignLog =
  withStandalone
    (field CampaignCampaignLog)
    (field ScenarioStandaloneCampaignLog)

getInvestigatorHasRecord :: HasGame m => InvestigatorId -> CampaignLogKey -> m Bool
getInvestigatorHasRecord iid k = fieldMap InvestigatorLog (hasRecord k) iid

getHasRecord :: HasGame m => CampaignLogKey -> m Bool
getHasRecord k = hasRecord k <$> getCampaignLog

hasRecord :: CampaignLogKey -> CampaignLog -> Bool
hasRecord k campaignLog =
  or
    [ k `member` campaignLogRecorded campaignLog
    , k `member` campaignLogRecordedCounts campaignLog
    ]

whenHasRecord :: HasGame m => CampaignLogKey -> m () -> m ()
whenHasRecord k = whenM (getHasRecord k)

unlessHasRecord :: HasGame m => CampaignLogKey -> m () -> m ()
unlessHasRecord k = unlessM (getHasRecord k)

getRecordCount :: HasGame m => CampaignLogKey -> m Int
getRecordCount k =
  findWithDefault 0 k . campaignLogRecordedCounts <$> getCampaignLog

getRecordSet :: HasGame m => CampaignLogKey -> m [SomeRecorded]
getRecordSet k =
  findWithDefault [] k . campaignLogRecordedSets <$> getCampaignLog

inRecordSet :: (Recordable a, HasGame m) => a -> CampaignLogKey -> m Bool
inRecordSet v k = do
  recordSet <- getRecordSet k
  pure $ recorded v `elem` recordSet

getCircledRecord :: forall a m. (Recordable a, HasGame m) => CampaignLogKey -> m (Maybe a)
getCircledRecord k = do
  rs <- getRecordSet k
  pure $ case mapMaybe isCircled rs of
    (x : _) -> Just x
    _ -> Nothing
 where
  isCircled = \case
    SomeRecorded _ (Circled (Recorded a :: Recorded b)) -> case eqT @a @b of
      Just Refl -> Just a
      Nothing -> Nothing
    _ -> Nothing

getRecordedCardCodes :: HasGame m => CampaignLogKey -> m [CardCode]
getRecordedCardCodes k = mapMaybe onlyRecorded <$> getRecordSet k
 where
  onlyRecorded :: SomeRecorded -> Maybe CardCode
  onlyRecorded = \case
    SomeRecorded RecordableCardCode (Recorded cCode) -> Just cCode
    _ -> Nothing

getCrossedOutCardCodes :: HasGame m => CampaignLogKey -> m [CardCode]
getCrossedOutCardCodes k = mapMaybe onlyCrossedOut <$> getRecordSet k
 where
  onlyCrossedOut :: SomeRecorded -> Maybe CardCode
  onlyCrossedOut = \case
    SomeRecorded RecordableCardCode (CrossedOut cCode) -> Just cCode
    _ -> Nothing

remembered :: HasGame m => ScenarioLogKey -> m Bool
remembered k = member k <$> scenarioField ScenarioRemembered

scenarioCount :: HasGame m => ScenarioCountKey -> m Int
scenarioCount k = fromMaybe 0 . lookup k <$> scenarioField ScenarioCounts

recordSetInsert
  :: (Recordable a, MonoFoldable t, Element t ~ a)
  => CampaignLogKey
  -> t
  -> Message
recordSetInsert k xs = RecordSetInsert k $ map recorded $ toList xs

recordSetReplace :: CampaignLogKey -> SomeRecorded -> SomeRecorded -> Message
recordSetReplace k v v' = RecordSetReplace k v v'

crossOutRecordSetEntries :: Recordable a => CampaignLogKey -> [a] -> Message
crossOutRecordSetEntries k xs = CrossOutRecordSetEntries k $ map recorded xs
