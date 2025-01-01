module Arkham.Event.Events.GetOverHere (
  getOverHere,
  GetOverHere (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.SkillType

newtype GetOverHere = GetOverHere EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getOverHere :: EventCard GetOverHere
getOverHere = event GetOverHere Cards.getOverHere

instance RunMessage GetOverHere where
  runMessage msg e@(GetOverHere attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- getJustLocation iid
      let m = LocationWithId lid
      enemies <-
        select
          $ NonEliteEnemy
          <> EnemyAt
            (LocationMatchAny [m, ConnectedFrom m, LocationWithDistanceFrom 2 m Anywhere])
      player <- getPlayer iid
      sid <- getRandom
      pushAll
        [ chooseOne
            player
            [ targetLabel
              enemy
              [ EnemyEngageInvestigator enemy iid
              , FightEnemy sid iid enemy (toSource attrs) Nothing SkillCombat False
              ]
            | enemy <- enemies
            ]
        ]
      pure e
    _ -> GetOverHere <$> runMessage msg attrs
