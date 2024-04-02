module Arkham.Asset.Cards.FireExtinguisher1 where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
import Arkham.Fight
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Prelude

newtype FireExtinguisher1 = FireExtinguisher1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireExtinguisher1 :: AssetCard FireExtinguisher1
fireExtinguisher1 = asset FireExtinguisher1 Cards.fireExtinguisher1

instance HasAbilities FireExtinguisher1 where
  getAbilities (FireExtinguisher1 a) =
    [ restrictedAbility a 1 ControlsThis fightAction_
    , restrictedAbility a 2 ControlsThis $ evadeAction (ExileCost $ toTarget a)
    ]

instance RunMessage FireExtinguisher1 where
  runMessage msg a@(FireExtinguisher1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifier source iid (SkillModifier #combat 1), chooseFight]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      pushAll
        [ skillTestModifier source iid (SkillModifier #agility 3)
        , createCardEffect Cards.fireExtinguisher1 Nothing source SkillTestTarget
        , chooseEvadeEnemy iid source #agility
        ]
      pure a
    _ -> FireExtinguisher1 <$> runMessage msg attrs

newtype FireExtinguisher1Effect = FireExtinguisher1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireExtinguisher1Effect :: EffectArgs -> FireExtinguisher1Effect
fireExtinguisher1Effect = cardEffect FireExtinguisher1Effect Cards.fireExtinguisher1

instance RunMessage FireExtinguisher1Effect where
  runMessage msg e@(FireExtinguisher1Effect attrs) = case msg of
    PassedSkillTest iid (Just Action.Evade) _ (Initiator (EnemyTarget _)) _ _ | SkillTestTarget == attrs.target -> do
      evasions <- selectMap (EnemyEvaded iid) $ enemyEngagedWith iid
      pushAll $ evasions <> [disable attrs]
      pure e
    SkillTestEnds _ _ -> e <$ push (disable attrs)
    _ -> FireExtinguisher1Effect <$> runMessage msg attrs
