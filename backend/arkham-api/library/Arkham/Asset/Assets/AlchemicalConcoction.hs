module Arkham.Asset.Assets.AlchemicalConcoction (alchemicalConcoction, AlchemicalConcoction (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype AlchemicalConcoction = AlchemicalConcoction AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalConcoction :: AssetCard AlchemicalConcoction
alchemicalConcoction = asset AlchemicalConcoction Cards.alchemicalConcoction

instance HasAbilities AlchemicalConcoction where
  getAbilities (AlchemicalConcoction a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage AlchemicalConcoction where
  runMessage msg a@(AlchemicalConcoction attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- aspect iid source (#intellect `InsteadOf` #combat) (mkChooseFight sid iid source)
      pushAll $ leftOr chooseFight
      pure a
    ChoseEnemy sid iid (isAbilitySource attrs 1 -> True) eid -> do
      isTheExperiment <- eid <=~> enemyIs Enemies.theExperiment
      when isTheExperiment $ pushM $ skillTestModifier sid (attrs.ability 1) iid (DamageDealt 6)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ RemoveFromGame (toTarget attrs)
      pure a
    _ -> AlchemicalConcoction <$> runMessage msg attrs
