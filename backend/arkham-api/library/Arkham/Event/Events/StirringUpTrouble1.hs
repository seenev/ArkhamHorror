module Arkham.Event.Events.StirringUpTrouble1 (stirringUpTrouble1, StirringUpTrouble1 (..)) where

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype StirringUpTrouble1 = StirringUpTrouble1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stirringUpTrouble1 :: EventCard StirringUpTrouble1
stirringUpTrouble1 = event StirringUpTrouble1 Cards.stirringUpTrouble1

instance RunMessage StirringUpTrouble1 where
  runMessage msg e@(StirringUpTrouble1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation attrs 2
      pure e
    _ -> StirringUpTrouble1 <$> runMessage msg attrs
