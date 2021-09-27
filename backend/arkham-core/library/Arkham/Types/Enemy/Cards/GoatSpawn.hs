module Arkham.Types.Enemy.Cards.GoatSpawn
  ( goatSpawn
  , GoatSpawn(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Timing qualified as Timing

newtype GoatSpawn = GoatSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goatSpawn :: EnemyCard GoatSpawn
goatSpawn = enemy GoatSpawn Cards.goatSpawn (3, Static 3, 2) (1, 0)

instance HasAbilities GoatSpawn where
  getAbilities (GoatSpawn a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyDefeated Timing.When Anyone
      $ EnemyWithId
      $ toId a
    ]

instance EnemyRunner env => RunMessage env GoatSpawn where
  runMessage msg e@(GoatSpawn attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      investigatorIds <- getSetList $ enemyLocation attrs
      e <$ pushAll
        [ InvestigatorAssignDamage iid source DamageAny 0 1
        | iid <- investigatorIds
        ]
    _ -> GoatSpawn <$> runMessage msg attrs
