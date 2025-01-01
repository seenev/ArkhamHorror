module Arkham.Event.Events.DrawnToTheFlame where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message qualified as Msg

newtype DrawnToTheFlame = DrawnToTheFlame EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnToTheFlame :: EventCard DrawnToTheFlame
drawnToTheFlame = event DrawnToTheFlame Cards.drawnToTheFlame

instance RunMessage DrawnToTheFlame where
  runMessage msg e@(DrawnToTheFlame attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      pushAll
        [ drawEncounterCard iid attrs
        , Msg.DiscoverClues iid $ discoverAtYourLocation attrs 2
        ]
      pure e
    _ -> DrawnToTheFlame <$> runMessage msg attrs
