module Arkham.Types.Enemy.Cards.ScreechingByakhee
  ( screechingByakhee
  , ScreechingByakhee(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query

newtype ScreechingByakhee = ScreechingByakhee EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

screechingByakhee :: EnemyCard ScreechingByakhee
screechingByakhee = enemyWith
  ScreechingByakhee
  Cards.screechingByakhee
  (3, Static 4, 3)
  (1, 2)
  (preyL .~ LowestRemainingSanity)

instance HasCount RemainingSanity env InvestigatorId => HasModifiersFor env ScreechingByakhee where
  getModifiersFor _ target (ScreechingByakhee attrs) | isTarget attrs target =
    do
      sanities <- map unRemainingSanity
        <$> traverse getCount (setToList $ enemyEngagedInvestigators attrs)
      pure $ toModifiers attrs $ if any (<= 4) sanities
        then [EnemyFight 1, EnemyEvade 1]
        else []
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) =
    ScreechingByakhee <$> runMessage msg attrs
