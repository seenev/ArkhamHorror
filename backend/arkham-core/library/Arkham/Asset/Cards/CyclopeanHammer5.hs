module Arkham.Asset.Cards.CyclopeanHammer5 (cyclopeanHammer5, CyclopeanHammer5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype CyclopeanHammer5 = CyclopeanHammer5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanHammer5 :: AssetCard CyclopeanHammer5
cyclopeanHammer5 = asset CyclopeanHammer5 Cards.cyclopeanHammer5

instance HasAbilities CyclopeanHammer5 where

  getAbilities (CyclopeanHammer5 a) = [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage CyclopeanHammer5 where
  runMessage msg a@(CyclopeanHammer5 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifiers source iid [DamageDealt 1, AddSkillValue #willpower], chooseFight]
      --  $ [chooseOrRunOne player [targetLabel lid [EnemyMove eid lid] | lid <- locations] | notNull locations]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      pushWhen (n >= 3) $ skillTestModifier (attrs.ability 1) iid (DamageDealt 1)
      pure a
    _ -> CyclopeanHammer5 <$> runMessage msg attrs