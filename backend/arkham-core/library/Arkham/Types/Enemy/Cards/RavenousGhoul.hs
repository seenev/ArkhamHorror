module Arkham.Types.Enemy.Cards.RavenousGhoul
  ( ravenousGhoul
  , RavenousGhoul(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Prey

newtype RavenousGhoul = RavenousGhoul EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ravenousGhoul :: EnemyCard RavenousGhoul
ravenousGhoul = enemyWith
  RavenousGhoul
  Cards.ravenousGhoul
  (3, Static 3, 3)
  (1, 1)
  (preyL .~ LowestRemainingHealth)

instance EnemyRunner env => RunMessage env RavenousGhoul where
  runMessage msg (RavenousGhoul attrs) = RavenousGhoul <$> runMessage msg attrs
