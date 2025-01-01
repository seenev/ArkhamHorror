module Arkham.Event.Events.MoonlightRitual (
  moonlightRitual,
  MoonlightRitual (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype MoonlightRitual = MoonlightRitual EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlightRitual :: EventCard MoonlightRitual
moonlightRitual = event MoonlightRitual Cards.moonlightRitual

instance RunMessage MoonlightRitual where
  runMessage msg e@(MoonlightRitual attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      -- we assume that the only cards that are relevant here are assets,events, and investigators
      assets <-
        selectWithField AssetDoom $ assetControlledBy iid <> AssetWithAnyDoom
      events <-
        selectWithField EventDoom $ eventControlledBy iid <> EventWithAnyDoom
      investigatorDoomCount <- field InvestigatorDoom iid
      player <- getPlayer iid
      pushAll
        [ chooseOne player
            $ [ targetLabel
                iid
                [RemoveDoom (toSource attrs) (toTarget iid) investigatorDoomCount]
              | investigatorDoomCount > 0
              ]
            <> [ targetLabel aid [RemoveDoom (toSource attrs) (toTarget aid) assetDoomCount]
               | (aid, assetDoomCount) <- assets
               ]
            <> [ targetLabel eventId [RemoveDoom (toSource attrs) (toTarget eventId) eventDoomCount]
               | (eventId, eventDoomCount) <- events
               ]
        ]
      pure e
    _ -> MoonlightRitual <$> runMessage msg attrs
