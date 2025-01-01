module Arkham.Enemy.Cards.StealthyZoog (
  stealthyZoog,
  StealthyZoog (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype StealthyZoog = StealthyZoog EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

stealthyZoog :: EnemyCard StealthyZoog
stealthyZoog =
  enemyWith StealthyZoog Cards.stealthyZoog (1, Static 1, 3) (0, 1)
    $ (spawnAtL ?~ SpawnEngagedWith (InvestigatorWithLowestSkill #combat $ InvestigatorAt YourLocation))
    . (preyL .~ Prey (InvestigatorWithLowestSkill #combat UneliminatedInvestigator))

instance RunMessage StealthyZoog where
  runMessage msg (StealthyZoog attrs) =
    StealthyZoog <$> runMessage msg attrs
