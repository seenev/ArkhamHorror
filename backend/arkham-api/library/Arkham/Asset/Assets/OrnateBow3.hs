module Arkham.Asset.Assets.OrnateBow3 (ornateBow3, OrnateBow3 (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype OrnateBow3 = OrnateBow3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ornateBow3 :: AssetCard OrnateBow3
ornateBow3 = asset OrnateBow3 Cards.ornateBow3

instance HasAbilities OrnateBow3 where
  getAbilities (OrnateBow3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [Action.Fight] #agility
        $ ActionCost 1
        <> assetUseCost a Ammo 1
    , withTooltip "You nock another arrow"
        $ controlledAbility
          a
          2
          (exists $ AssetWithId (toId a) <> NotAsset (AssetWithUses Ammo))
          actionAbility
    ]

instance RunMessage OrnateBow3 where
  runMessage msg a@(OrnateBow3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- aspect iid source (#agility `InsteadOf` #combat) (mkChooseFight sid iid source)
      enabled <- skillTestModifiers sid source iid [DamageDealt 2, SkillModifier #agility 2]
      pushAll $ enabled : leftOr chooseFight
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ AddUses (attrs.ability 2) (toId attrs) Ammo 1
      pure a
    _ -> OrnateBow3 <$> runMessage msg attrs
