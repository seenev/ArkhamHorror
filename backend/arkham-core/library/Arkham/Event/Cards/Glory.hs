module Arkham.Event.Cards.Glory (
  glory,
  Glory (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype Glory = Glory EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glory :: EventCard Glory
glory = event Glory Cards.glory

instance RunMessage Glory where
  runMessage msg e@(Glory attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ drawCards iid attrs 2
      pure e
    _ -> Glory <$> runMessage msg attrs
