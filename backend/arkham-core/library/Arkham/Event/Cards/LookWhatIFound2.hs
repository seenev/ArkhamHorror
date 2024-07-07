module Arkham.Event.Cards.LookWhatIFound2 where

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype LookWhatIFound2 = LookWhatIFound2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lookWhatIFound2 :: EventCard LookWhatIFound2
lookWhatIFound2 = event LookWhatIFound2 Cards.lookWhatIFound2

instance RunMessage LookWhatIFound2 where
  runMessage msg e@(LookWhatIFound2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      pushAll
        [ ResolveEvent iid eid Nothing windows'
        , ResolveEvent iid eid Nothing windows'
        ]
      pure e
    ResolveEvent iid eid _ _ | eid == toId attrs -> do
      locations <- select $ oneOf [YourLocation, ConnectedLocation] <> LocationWithAnyClues
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel lid [Msg.DiscoverClues iid $ discover lid (toSource attrs) 1]
          | lid <- locations
          ]
      pure e
    _ -> LookWhatIFound2 <$> runMessage msg attrs
