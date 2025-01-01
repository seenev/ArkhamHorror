module Arkham.Event.Events.IfItBleeds (
  ifItBleeds,
  IfItBleeds (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype IfItBleeds = IfItBleeds EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ifItBleeds :: EventCard IfItBleeds
ifItBleeds = event IfItBleeds Cards.ifItBleeds

getWindowEnemyIds :: InvestigatorId -> [Window] -> [EnemyId]
getWindowEnemyIds iid = mapMaybe \case
  Window Timing.After (EnemyDefeated (Just who) _ eid) _ | iid == who -> Just eid
  _ -> Nothing

instance RunMessage IfItBleeds where
  runMessage msg e@(IfItBleeds attrs) = case msg of
    InvestigatorPlayEvent iid eid _ (getWindowEnemyIds iid -> enemyIds) _ | eid == toId attrs -> do
      choices <- for enemyIds $ \enemyId -> do
        horrorValue <- field EnemySanityDamage enemyId
        investigators <- select $ HealableInvestigator (toSource attrs) #horror $ colocatedWith iid
        pure
          $ targetLabel
            enemyId
            [HealHorror (toTarget investigator) (toSource attrs) horrorValue | investigator <- investigators]
      player <- getPlayer iid
      push $ chooseOne player choices
      pure e
    _ -> IfItBleeds <$> runMessage msg attrs
