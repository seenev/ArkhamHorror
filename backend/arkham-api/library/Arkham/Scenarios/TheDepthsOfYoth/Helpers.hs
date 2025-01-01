module Arkham.Scenarios.TheDepthsOfYoth.Helpers where

import Arkham.Prelude

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Window
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window
import Arkham.Zone
import Data.Aeson (Result (..))

newtype DepthsOfYothMeta = DepthsOfYothMeta
  { depthLocation :: LocationId
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

incrementDepth :: HasGame m => m [Message]
incrementDepth = do
  addingToCurrentDepth <-
    checkWindows
      [mkWindow Timing.When Window.AddingToCurrentDepth]
  pure [addingToCurrentDepth, ScenarioCountIncrementBy CurrentDepth 1]

getCurrentDepth :: HasGame m => m Int
getCurrentDepth = scenarioCount CurrentDepth

getDepthStart :: HasGame m => m LocationId
getDepthStart = depthLocation <$> getMeta

getMeta :: HasGame m => m DepthsOfYothMeta
getMeta = do
  v <- scenarioField ScenarioMeta
  case fromJSON v of
    Error _ -> error "invalid meta for depths of yoth"
    Success a -> pure a

toMeta :: LocationId -> Value
toMeta lid = toJSON $ DepthsOfYothMeta lid

getInPursuitEnemyWithHighestEvade
  :: HasGame m => m (Set EnemyId)
getInPursuitEnemyWithHighestEvade = do
  inPursuit <- getInPursuitEnemies
  evadeValue <-
    selectAgg' @(OutOfPlayEntity 'PursuitZone Enemy)
      (Max0 . fromMaybe 0)
      (OutOfPlayEnemyField PursuitZone EnemyEvade)
      (OutOfPlayEnemy PursuitZone $ EnemyOneOf $ map EnemyWithId $ toList inPursuit)
  setFromList
    <$> filterM
      ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
          (OutOfPlayEnemyField PursuitZone EnemyEvade)
          ((== Just evadeValue))
      )
      (toList inPursuit)

getInPursuitEnemies :: HasGame m => m [EnemyId]
getInPursuitEnemies = select $ OutOfPlayEnemy PursuitZone AnyEnemy

getPlacePursuitEnemyMessages :: HasGame m => m [Message]
getPlacePursuitEnemyMessages = do
  choices <- toList <$> getInPursuitEnemyWithHighestEvade
  lead <- getLeadPlayer
  depthStart <- getDepthStart
  pure $ do
    guard $ notNull choices
    pure
      $ chooseOrRunOne
        lead
        [ targetLabel choice [PlaceEnemy choice $ AtLocation depthStart]
        | choice <- choices
        ]

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theDepthsOfYoth" a
