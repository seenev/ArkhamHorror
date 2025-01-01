module Arkham.Enemy.Cards.RelentlessDarkYoung (relentlessDarkYoung, RelentlessDarkYoung (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype RelentlessDarkYoung = RelentlessDarkYoung EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relentlessDarkYoung :: EnemyCard RelentlessDarkYoung
relentlessDarkYoung =
  enemyWith RelentlessDarkYoung Cards.relentlessDarkYoung (4, Static 5, 2) (2, 1)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasAbilities RelentlessDarkYoung where
  getAbilities (RelentlessDarkYoung attrs) =
    extend attrs [mkAbility attrs 1 $ forced $ RoundEnds #when]

instance RunMessage RelentlessDarkYoung where
  runMessage msg e@(RelentlessDarkYoung attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ HealDamage (toTarget attrs) (toSource attrs) 2
      pure e
    _ -> RelentlessDarkYoung <$> runMessage msg attrs
