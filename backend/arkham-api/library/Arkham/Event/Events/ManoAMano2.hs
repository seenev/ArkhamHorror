module Arkham.Event.Events.ManoAMano2 (
  manoAMano2,
  ManoAMano2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype ManoAMano2 = ManoAMano2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manoAMano2 :: EventCard ManoAMano2
manoAMano2 = event ManoAMano2 Cards.manoAMano2

instance RunMessage ManoAMano2 where
  runMessage msg e@(ManoAMano2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemies <- select $ enemyEngagedWith iid
      player <- getPlayer iid
      pushAll
        [ chooseOrRunOne
            player
            [ targetLabel enemy [EnemyDamage enemy $ nonAttack attrs 2]
            | enemy <- enemies
            ]
        ]
      pure e
    _ -> ManoAMano2 <$> runMessage msg attrs
