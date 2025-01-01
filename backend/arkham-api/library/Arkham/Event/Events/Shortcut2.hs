module Arkham.Event.Events.Shortcut2 (
  shortcut2,
  Shortcut2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Movement
import Arkham.Placement

newtype Shortcut2 = Shortcut2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shortcut2 :: EventCard Shortcut2
shortcut2 = event Shortcut2 Cards.shortcut2

instance HasAbilities Shortcut2 where
  getAbilities (Shortcut2 a) = case eventAttachedTarget a of
    Just (LocationTarget lid) ->
      [ restrictedAbility
          (proxied lid a)
          1
          (OnLocation (LocationWithId lid) <> LocationExists AccessibleLocation)
          (FastAbility' (exhaust a) [#move])
      ]
    _ -> []

instance RunMessage Shortcut2 where
  runMessage msg e@(Shortcut2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- selectJust $ locationWithInvestigator iid
      push $ PlaceEvent eid (AttachedToLocation lid)
      pure e
    UseThisAbility iid (ProxySource _ (isSource attrs -> True)) 1 -> do
      connectingLocations <- getAccessibleLocations iid attrs
      player <- getPlayer iid
      push $ chooseOne player $ targetLabels connectingLocations (only . Move . move (toSource attrs) iid)
      pure e
    _ -> Shortcut2 <$> runMessage msg attrs
