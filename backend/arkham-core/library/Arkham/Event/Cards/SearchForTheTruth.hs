module Arkham.Event.Cards.SearchForTheTruth where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype SearchForTheTruth = SearchForTheTruth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheTruth :: EventCard SearchForTheTruth
searchForTheTruth = event SearchForTheTruth Cards.searchForTheTruth

instance RunMessage SearchForTheTruth where
  runMessage msg e@(SearchForTheTruth attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      x <- fieldMap InvestigatorClues (min 5) iid
      push $ drawCards iid attrs x
      pure e
    _ -> SearchForTheTruth <$> runMessage msg attrs
