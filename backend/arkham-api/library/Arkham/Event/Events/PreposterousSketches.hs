module Arkham.Event.Events.PreposterousSketches (
  preposterousSketches,
  PreposterousSketches (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype PreposterousSketches = PreposterousSketches EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preposterousSketches :: EventCard PreposterousSketches
preposterousSketches = event PreposterousSketches Cards.preposterousSketches

instance RunMessage PreposterousSketches where
  runMessage msg e@(PreposterousSketches attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ drawCards iid attrs 3
      pure e
    _ -> PreposterousSketches <$> runMessage msg attrs
