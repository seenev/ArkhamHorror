module Arkham.Event.Events.BellyOfTheBeast (bellyOfTheBeast, BellyOfTheBeast (..)) where

import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Id
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype BellyOfTheBeast = BellyOfTheBeast EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bellyOfTheBeast :: EventCard BellyOfTheBeast
bellyOfTheBeast = event BellyOfTheBeast Cards.bellyOfTheBeast

toEnemyId :: [Window] -> EnemyId
toEnemyId [] = error "Invalid window"
toEnemyId ((windowType -> Window.SuccessfulEvadeEnemy _ enemyId _) : _) = enemyId
toEnemyId (_ : xs) = toEnemyId xs

instance RunMessage BellyOfTheBeast where
  runMessage msg e@(BellyOfTheBeast attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ (toEnemyId -> enemyId) _ | eid == toId attrs -> do
      mlid <- selectOne $ locationWithEnemy enemyId
      for_ mlid $ \lid -> push $ Msg.DiscoverClues iid $ discover lid attrs 1
      pure e
    _ -> BellyOfTheBeast <$> liftRunMessage msg attrs
