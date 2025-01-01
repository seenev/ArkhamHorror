module Arkham.Enemy.Cards.LupineThrall (LupineThrall (..), lupineThrall) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype LupineThrall = LupineThrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lupineThrall :: EnemyCard LupineThrall
lupineThrall =
  enemyWith LupineThrall Cards.lupineThrall (4, Static 3, 4) (1, 1)
    $ (preyL .~ Prey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator))
    . (spawnAtL ?~ SpawnAt (FarthestLocationFromYou Anywhere))

instance RunMessage LupineThrall where
  runMessage msg (LupineThrall attrs) = LupineThrall <$> runMessage msg attrs
