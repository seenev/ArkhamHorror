module Arkham.Enemy.Cards.CloverClubPitBoss (CloverClubPitBoss (..), cloverClubPitBoss) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype CloverClubPitBoss = CloverClubPitBoss EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubPitBoss :: EnemyCard CloverClubPitBoss
cloverClubPitBoss =
  enemyWith CloverClubPitBoss Cards.cloverClubPitBoss (3, Static 4, 3) (2, 0)
    $ preyL
    .~ Prey (InvestigatorWithHighestSkill #intellect UneliminatedInvestigator)

instance HasAbilities CloverClubPitBoss where
  getAbilities (CloverClubPitBoss x) =
    extend
      x
      [ restrictedAbility x 1 OnSameLocation
          $ forced
          $ GainsClues #after You AnyValue
      ]

instance RunMessage CloverClubPitBoss where
  runMessage msg e@(CloverClubPitBoss attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        $ [Ready (toTarget attrs) | enemyExhausted attrs]
        <> [ EnemyEngageInvestigator (toId attrs) iid
           , EnemyAttackIfEngaged (toId attrs) (Just iid)
           ]
      pure e
    _ -> CloverClubPitBoss <$> runMessage msg attrs
