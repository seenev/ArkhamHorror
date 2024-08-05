module Arkham.Treachery.Cards.EncephalonSignal (encephalonSignal, EncephalonSignal (..)) where

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Trait (Trait (Humanoid))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype EncephalonSignal = EncephalonSignal TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encephalonSignal :: TreacheryCard EncephalonSignal
encephalonSignal = treachery EncephalonSignal Cards.encephalonSignal

instance RunMessage EncephalonSignal where
  runMessage msg t@(EncephalonSignal attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mOtherworldlyMeddler <- selectOne $ enemyIs Enemies.otherworldlyMeddler
      sid <- getRandom
      pushAll
        $ [ PlaceDoom (toSource attrs) (toTarget otherworldlyMeddler) 1
          | otherworldlyMeddler <- toList mOtherworldlyMeddler
          ]
        <> [revelationSkillTest sid iid attrs #willpower (Fixed 4)]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      player <- getPlayer iid
      investigators <- select $ colocatedWith iid
      humanoids <- select $ enemyAtLocationWith iid <> EnemyWithTrait Humanoid
      push
        $ chooseOne
          player
          [ Label "Take 2 horror" [assignHorror iid (toSource attrs) 2]
          , Label
              "Deal 2 damage to an investigator or Humanoid enemy at your location"
              [ chooseOrRunOne player
                  $ [ targetLabel investigator [assignDamage investigator (toSource attrs) 2]
                    | investigator <- investigators
                    ]
                  <> [targetLabel humanoid [EnemyDamage humanoid $ nonAttack (toSource attrs) 2] | humanoid <- humanoids]
              ]
          ]
      pure t
    _ -> EncephalonSignal <$> runMessage msg attrs
