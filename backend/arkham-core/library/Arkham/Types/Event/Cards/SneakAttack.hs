module Arkham.Types.Event.Cards.SneakAttack where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype SneakAttack = SneakAttack EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sneakAttack :: EventCard SneakAttack
sneakAttack = event SneakAttack Cards.sneakAttack

instance EventRunner env => RunMessage env SneakAttack where
  runMessage msg e@(SneakAttack attrs) = case msg of
    InvestigatorPlayEvent you eid _ _ | eid == toId attrs -> do
      yourLocation <- LocationWithId <$> getId you
      enemies <- selectList $ ExhaustedEnemy <> EnemyAt yourLocation
      e <$ pushAll
        ([ EnemyDamage enemy you (toSource attrs) 2 | enemy <- enemies ]
        <> [Discard $ toTarget attrs]
        )
    _ -> SneakAttack <$> runMessage msg attrs
